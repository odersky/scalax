package scalax.collection

/** Worksheet to test views */
object views {
  import FoldingViews._
  
  val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)    //> xs  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  val vs0 = View(xs)                              //> vs0  : scalax.collection.FoldingViews.View[Int]{type A = Int; def transforme
                                                  //| r: scalax.collection.FoldingViews.IdentityFT[this.A]} = View(1, 2, 3, 4, 5, 
                                                  //| 6, 7, 8, 9, 10)
                           
                        
  val vs1 = vs0 map (_ + 1)                       //> vs1  : scalax.collection.FoldingViews.View[Int] = View(2, 3, 4, 5, 6, 7, 8, 
                                                  //| 9, 10, 11)
  val vs2 = vs1 filter (_ % 2 == 0)               //> vs2  : scalax.collection.FoldingViews.View[Int] = View(2, 4, 6, 8, 10)
  
  val vs3 = vs2 flatMap (x => View(x :: x :: Nil))//> vs3  : scalax.collection.FoldingViews.View[Int]{type A = scalax.collection.v
                                                  //| iews.vs2.A} = View(2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
                
  val vs4 = vs3 take 3                            //> vs4  : scalax.collection.FoldingViews.View[Int]{type A = scalax.collection.v
                                                  //| iews.vs3.A} = View(2, 2, 4)
                   
                                                  
                                                  
  val vs5 = vs4 flatMap (x => View(1 until x))    //> vs5  : scalax.collection.FoldingViews.View[Int]{type A = scalax.collection.v
                                                  //| iews.vs4.A} = View(1, 1, 1, 2, 3)
  val vs6 = vs3 drop 3                            //> vs6  : scalax.collection.FoldingViews.View[Int]{type A = scalax.collection.v
                                                  //| iews.vs3.A} = View(4, 6, 6, 8, 8, 10, 10)
  
  val vs7 = vs6 map (x => x * x)                  //> vs7  : scalax.collection.FoldingViews.View[Int] = View(16, 36, 36, 64, 64, 1
                                                  //| 00, 100)
  vs7 find (_ % 10 == 0)                          //> res0: Option[Int] = Some(100)
  
  val ws1 = xs takeWhile (_ % 4 != 0)             //> ws1  : List[Int] = List(1, 2, 3)
  val ws3 = xs dropWhile (_ % 4 != 0)             //> ws3  : List[Int] = List(4, 5, 6, 7, 8, 9, 10)
  val s = vs0.zipWithIndex.toStream               //> s  : Stream[(Int, Int)] = Stream((1,0), ?)
 	s.take(3).toList                          //> res1: List[(Int, Int)] = List((1,0), (2,1), (3,2))
  vs0 indexOf (_ % 4 == 0)                        //> res2: Int = 3
}