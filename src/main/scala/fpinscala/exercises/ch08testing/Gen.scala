package fpinscala.exercise.ch08testing

/**
 * Exercise 8.1
 *
 * To get used to thinking about testing in this way,
 * come up with properties that specify the implementation of a sum:
 * List[Int] => Int function.
 * You don't have to write your properties down as executable ScalaCheck code
 * an informal description is fine.
 * Here are some ideas to get you started:
 *
 * 1. Reversing a list and summing it
 *    should give the same result as summing the original, nonreversed list.
 * 2. What should the sum be if all elements of the list are the same value?
 * 3. Can you think of other properties?
 */
// for 2, all the element should equals to the sum / list.length
// 4. multiple each of the List element
//    will get a sum of n * sum of the original List

/**
 * Exercise 8.2
 *
 * What properties specify a function that finds the maximum of a List[Int]?
 */
// 1. max should larger than all of the elements in the list.
// 2. max of the reverse list should equals to the max of the original list.
