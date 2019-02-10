
import scala.math
import scala.util.Random

object PSO {
  def main(args: Array[String]) {
        println("Hello, world!")
        val(x,y) = (12,13) 
        val UpperBound= 10
        val LowerBound = -10
       val population_size = 17
        val maxiteration = 30
        val obj1 = new Simulations(spheref(6),x, UpperBound, LowerBound, population_size,maxiteration)
        val obj2 = new Simulations(ackeleyf(15),x, UpperBound, LowerBound, population_size,maxiteration)
        val obj3 = new Simulations(sumOfSquaresf(6),x, UpperBound, LowerBound, population_size,maxiteration)

      }
  def spheref(x : Int) : Int =  {
    var sum = 0
    for(i <- 1 to x)
      sum = sum + x(i) * 2
      return sum
  
  }  
  def ackeleyf(x: Int) : Double = {
    var a= 20
    var b = 0.2
    var c = 2.0 * 3.1428
    var sum1 = 0.0
    var sum2 = 0.0
    for (i <- 1 to x){
      sum1 = sum1 + x(i) * 2.0
      sum2 = sum2 + math.cos(c * x(i))
    }
    var d = x
      return -a * math.exp(-b * math.sqrt(sum1 / d)) - math.exp(sum2 / d) + a + math.E
    
  }
  def sumOfSquaresf(x : Int) : Int = {
    var sum = 0
    for (i <- 1 to x){
      sum = sum + i * x(i) * 2
    }
    return sum
  }
  class Simulations(y : Int, x : Int,Lowerbound: Int, UpperBound : Int, population_size : Int, maxiteration : Int){
   
   var err_best_g = -1                   
   var pos_best_g= List(Nil) 
   var Dimensions : Int = x
   var global_best_cost = -1
   var Low : Int = -10
   var High : Int = 10
   var population_size : Int = 10
   var maxiteration : Int = 20
   
   var swarm = List(Nil)
        for (i <- 0 to population_size){
            swarm = i :: swarm
        }
   var i=0
        while (i < maxiteration){
            for (j <- 0 to population_size){
              swarm(j).evaluate(LowerBound, UpperBound)

                // determine if current particle is the global best
                if (swarm(j).err_i < err_best_g or err_best_g == -1){
                    pos_best_g= List(swarm(j).position)
                    err_best_g= (swarm(j).err_i)
            }
            
        }
        
   for (j <- 0 to population_size)
                swarm(j).update_velocity(pos_best_g)
                swarm(j).update_position(LowerBound)
            i+=1

        
        println( "Results are:")
        println (pos_best_g)
        println (err_best_g)

  }
  }
  class Particle() extends Simulations {
     val position = List(Nil)          
     val velocity = List(Nil)        
     val best = List(Nil)          
     val err_best = -1          
     val err_i = -1               

        for (i <- 0 to Dimensions){
          
            val r = new scala.util.Random
            val start: Double = -1
            val end: Double   = 1
            val r1 = between(start, end, r)
            val rr = math.abs(r1)
            velocity.append (rr)
            position.append(rr)
        }
     def between(low: Double, high: Double, r: Random): Double = {
       if (low == high) {
         low
         } 
       else {
         val mid = low + (high/2 - low/2)
         if (r.nextBoolean) between(low, mid, r)
         else between(mid, high, r)
       }
   }
     def evaluate(y : Int , UpperBound : Int)
        err_i= UpperBound[position]

        //check to see if the current position is an individual best
        if (err_i < err_best){
            best = position
            err_best = err_i
        }
     def update_velocity(y: Int , bestPostion : Int){
        val weight : Double = 0.5      
        val c1 = 2       
        val c2 = 2        
        for (i <- 0 to population_size){
            val r1 = new scala.util.Random
            val r2 = new scala.util.Random
            val Nostalgia= (c1) * (r1)* (bestPosition - position(i))
            val Norm= c2 * r2* (pos_best_g - position(i))
            val velocity(i) = weight * velocity(i)+ Nostalgia+ Norm
        }
     }
     def updatePosition(y : Int, bounds : Int){
        for (i <- 0 to population_size){
            position(i) = position(i) + velocity(i)

            if position(i) > bounds(i)[1]
                position(i) = bounds(i)[1]

            if position(i) < bounds(i)[0]
                position(i) = bounds(i)[0]
        }
     }
  }
}