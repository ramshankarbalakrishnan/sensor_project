import java.io.File
import scala.collection.mutable
object sensor_metrics  {
  def main(args: Array[String])  = {
    var num_files=0
    var msr_processed=0
    var num_of_failed_msrmnt=0
    var nan_sensor: Array[String] = Array()
    var sensor_map: mutable.Map[String,Array[Int]]= mutable.Map()
    var sensor_map_sorted: mutable.Map[String,String]= mutable.Map()
    println("Enter the directory full path: ")
    val directory_path = new File(scala.io.StdIn.readLine())
    val file_list= directory_path.listFiles.filter(_.isFile).filter(_.getName.endsWith(".csv")).map(_.getPath).toList
    file_list.foreach{files =>  {
      num_files+=1
      val bufferedSource = io.Source.fromFile(files)
      var count=0
      for (line <- bufferedSource.getLines.drop(1)) {
        val line_arr: Array[String] = line.split(",").map(_.trim)
        var signal = sensor_map.contains(line_arr(0)) match {
          case true => line_arr(1) match {
            case "NaN" => {
              //sensor_map.put(line_arr(0),sensor_map.get(line_arr(0)).get :+ 0)
              num_of_failed_msrmnt += 1
            }
            case _ => sensor_map.put(line_arr(0),sensor_map.get(line_arr(0)).get :+ line_arr(1).toInt)
          }
          case false => line_arr(1) match {
            case "NaN" => {
              //sensor_map.put(line_arr(0),sensor_map.get(line_arr(0)).get :+ 0)
              num_of_failed_msrmnt += 1
            }
            case _ => sensor_map.put(line_arr(0),Array(line_arr(1).toInt))
          }
        }
        if(!sensor_map.contains(line_arr(0)) && (!(nan_sensor.contains(line_arr(0))))) {
          nan_sensor=nan_sensor :+ line_arr(0)
        }
        count+=1
        msr_processed+=1
      }
      //println("Count of records in file " + files + ": " + count)
    }

    }
    val sensor_details_list = sensor_map.toList
    println("Number of processed files: " + num_files)
    println("Number of measurements processed: " + msr_processed)
    println("Number of failed measurements: " + num_of_failed_msrmnt)
    println("Sensors with highest avg humidity:\n\nsensor-id,min,avg,max")

    sensor_map.foreach( sensor => {
      val value_arr=sensor._2
      val value_arr_len = value_arr.length
      var value_sum = 0
      var value_avg = 0
      if(value_arr_len != 0) {
        val min_val = value_arr.min
        val max_val = value_arr.max
        for (i <- 0 to value_arr_len - 1) {
          value_sum += value_arr(i)
        }
        value_avg = value_sum / value_arr_len
        val sensor_key = sensor._1.toString
        val value_avg_str = value_avg.toString
        sensor_map_sorted.put(value_avg_str + sensor_key, sensor_key + "," + min_val.toString + "," + value_avg_str + "," + max_val.toString)
      }
    })
    val sensor_map_sorted_ls = List(sensor_map_sorted.toSeq.sortBy(_._1).reverse:_*)
    sensor_map_sorted_ls.map(x=>x._2).foreach(println)
    nan_sensor.foreach(nan_sensor => {
      println(nan_sensor + ",NaN,NaN,NaN")
    })
  }
}