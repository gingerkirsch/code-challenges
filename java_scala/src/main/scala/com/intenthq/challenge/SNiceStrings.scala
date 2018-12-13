package com.intenthq.challenge

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?


  // val regex = "^(.*[aeiou].*[aeiou].*[aeiou])$|^(.)\\1{2,}+$|^((?!(ab|cd|pq|xy)).)*$" to avoid triple match - needs debug
  val regex1 = "^(.*[aeiou].*[aeiou].*[aeiou])$"
  val regex2 = "^(.)\\1{2,}+$"
  val regex3 = "^((?!(ab|cd|pq|xy)).)*$"

  def nice(xs: List[String]): Int = xs.filter(str => str.matches(regex1) & str.matches(regex2) & str.matches(regex3)).size
}
