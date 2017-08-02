package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
    *
    *  original:
    *  //def wordOccurrences(w: Word): Occurrences = ???
   */
  def wordOccurrences(w: Word): Occurrences = (w.toLowerCase groupBy (c => c) map { case(c,s) => (c, s.length) } toList) sorted
  /*
  last_version:
  def wordOccurrences(w: Word): Occurrences = (w.toLowerCase groupBy (c => c) map { case(c,s) => (c, s.length) } toList) sorted

  3rd_version:
  def wordOccurrences(w: Word): Occurrences = ((w.toLowerCase groupBy (c => c) ) toList ) map { case(c,s) => (c, s.length) } sorted

  2nd_version:
  def wordOccurrences(w: Word): Occurrences = ((w.toLowerCase groupBy (c => c) ) toList ) map (t => (t._1, t._2.length) )

  1st_ugly_version:
  def wordOccurrences(w: Word): Occurrences = {
    def mapTuple ( t: (Char, String) ) : (Char, Int)  = t match {
      case (c, s) => (c, s.length)
    }
    val myList: List[(Char, String)] = (w.toLowerCase groupBy (c => c) ) toList

    (for ( tuple <- myList) yield mapTuple(tuple)).sorted
  }
  */

  /** Converts a sentence into its character occurrence list.
    * original:
    * def sentenceOccurrences(s: Sentence): Occurrences = ???
    */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences (s mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
    * original:
    * lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = ???
    *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = (for (
      word <- dictionary;
      occurrence =  wordOccurrences(word)
    ) yield (occurrence, word)) groupBy { case (occurrence, list) => occurrence } map { case (occurrence, list) => (occurrence, list map { case (ocurrence, word) => word} ) } withDefaultValue(List())


  /** Returns all the anagrams of a given word.
    * original: def wordAnagrams(word: Word): List[Word] = ???
    */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences (wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
    *
    *  original: def combinations(occurrences: Occurrences): List[Occurrences] = ???
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def isValid(occurrences: Occurrences): Boolean = occurrences groupBy {case(c,f) => c } forall { case (c, l) => l.length ==1}
    def sortOccurrence(o:Occurrences): Occurrences = o.sortWith((t1, t2) => t1._1 < t2._1)
    val list = (for ((c, f) <- occurrences;r <- (1 to f);charFrequency = (c, r)) yield charFrequency)
    val occurrencesResult :List[Occurrences] = list.toSet.subsets().map(e => e.toList).toSet.toList filter(isValid)
    occurrencesResult.map(o=>sortOccurrence(o))//.filter(o => !o.isEmpty)
  }
  /*
  1st_version:

  the key is the use of the method subsets() but it is also the problem

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def isValid(occurrences: Occurrences): Boolean = occurrences groupBy {case(c,f) => c } forall { case (c, l) => l.length ==1}
    val list = (for ((c, f) <- occurrences;r <- (1 to f);charFrequency = (c, r)) yield charFrequency)
    list.toSet.subsets().map(e => e.toList).toSet.toList filter(isValid)
  }
  */

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
    *
    *  Hint: you can use `foldLeft`, and `-`, `apply` and `updated` operations on `Map`.
    *
    *  original: def subtract(x: Occurrences, y: Occurrences): Occurrences = ???
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def getFrequency(c:Char): Int = {
      val z = y.filter{ case(c1,f1) => c == c1}
      if(z.isEmpty) 0
      else z.head._2
    }
    x.map{ case(c,f) => (c, (f - getFrequency(c)))}.filter{ case(x,y) => y >0}
  }
  /*
  1st_version:
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def getFrequency(c:Char): Int = {
      val z = y.filter{ case(c1,f1) => c == c1}
      if(z.isEmpty) 0
      else z.head._2
    }
    x.map{ case(c,f) => (c, (f - getFrequency(c)))}.filter{ case(x,y) => y >0}
  }
  */

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
    *
    *  /*An anagram of a sentence is formed by taking the occurrences of all the characters of
    *  all the words in the sentence, and producing all possible combinations of words with those characters,
    *  such that the words have to be from the dictionary.*/
    *
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    if(sentence.isEmpty) List(Nil)
    else {
      def sentenceOccurrencesAux(sentence: Sentence):Occurrences=sentenceOccurrences(sentence.map(w => w.toLowerCase))
      val sentenceOccurrence = sentenceOccurrencesAux(sentence)

      def joinAndCompareOccurrences(list1: List[Sentence], list2: List[Sentence]):Boolean={
        val list3:List[Sentence] = list1 ++ list2
        val result:Sentence = list3.map(e => e.map(w => w.toString)).flatten
        sentenceOccurrence==sentenceOccurrencesAux(result)
      }

      def join(s1:Sentence, s2:Sentence): Sentence = if (joinAndCompareOccurrences(List(s1), List(s2))) s1 ::: s2 else List()

      def loop(listSent: List[Sentence]): List[Sentence] = {
        if (listSent.isEmpty) List()
        else {
          val listJoin2:List[Sentence] =
          (for (
            sent1 <- listSent;
            sent2 <- listSent;
            result = join(sent1, sent2)
          ) yield result).toSet.toList.filter(e => !e.isEmpty)

          val listJoin3:List[Sentence] =
            (for (
              sent1 <- listSent;
              sent2 <- listSent;
              sent3 <- listSent;
              result = join(sent1, sent2:::sent3)
            ) yield result).toSet.toList.filter(e => !e.isEmpty)

          listJoin2:::listJoin3
        }
      }

      def flatSentences1Word(sents: List[Sentence]): List[Sentence] = sents.map(s => s.map(w => List(w))).flatten

      val sentenceOcurrences = combinations(sentenceOccurrence)
      val sentences:List[Sentence] = (for (oc <- sentenceOcurrences;
                                           words = dictionaryByOccurrences(oc)
      ) yield words).toSet.filter(s => !s.isEmpty).toList

      loop(flatSentences1Word(sentences))
    }
  }

  /*
    type Word = String
    type Sentence = List[Word]
    type Occurrences = List[(Char, Int)]
    val dictionary: List[Word] = loadDictionary
    def wordOccurrences(w: Word): Occurrences
    def sentenceOccurrences(s: Sentence): Occurrences
    lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]]
    def wordAnagrams(word: Word): List[Word]
    def combinations(occurrences: Occurrences): List[Occurrences]
    def subtract(x: Occurrences, y: Occurrences): Occurrences
  */

  /*
  1st_fail_version:
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if( sentence.isEmpty ) List(Nil)
    else {
      val occurrencesXword = sentence map wordOccurrences
      val sentences = (for (
        occurrence <- occurrencesXword;
        occurrenceSubSet <- combinations(occurrence);
        sentence = dictionaryByOccurrences(subtract(occurrence, occurrenceSubSet))
      ) yield sentence)

      sentences
    }
  }

  2nd_version:
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    if(sentence.isEmpty) List(Nil)
    else {
      def sentenceOccurrencesAux(sentence: Sentence):Occurrences=sentenceOccurrences(sentence.map(w => w.toLowerCase))
      val sentenceOccurrence = sentenceOccurrencesAux(sentence)

      def joinAndCompareOccurrences(list1: List[Sentence], list2: List[Sentence]):Boolean={
        val list3:List[Sentence] = list1 ++ list2
        val result:Sentence = list3.map(e => e.map(w => w.toString)).flatten
        sentenceOccurrence==sentenceOccurrencesAux(result)
      }

      def join(s1:Sentence, s2:Sentence): Sentence = if (joinAndCompareOccurrences(List(s1), List(s2))) s1 ::: s2 else List()

      def loop(listSent: List[Sentence]): List[Sentence] = {
        if (listSent.isEmpty) List()
        else {
          val listJoin2:List[Sentence] =
          (for (
            sent1 <- listSent;
            sent2 <- listSent;
            result = join(sent1, sent2)
          ) yield result).toSet.toList.filter(e => !e.isEmpty)

          val listJoin3:List[Sentence] =
            (for (
              sent1 <- listSent;
              sent2 <- listSent;
              sent3 <- listSent;
              result = join(sent1, sent2:::sent3)
            ) yield result).toSet.toList.filter(e => !e.isEmpty)

          listJoin2:::listJoin3
        }
      }

      def flatSentences1Word(sents: List[Sentence]): List[Sentence] = sents.map(s => s.map(w => List(w))).flatten

      val sentenceOcurrences = combinations(sentenceOccurrence)
      val sentences:List[Sentence] = (for (oc <- sentenceOcurrences;
                                           words = dictionaryByOccurrences(oc)
      ) yield words).toSet.filter(s => !s.isEmpty).toList

      loop(flatSentences1Word(sentences))
    }
  }

  */


  /*

1st_submmit:

Your overall score for this assignment is 9.08 out of 10.00

[Test Description] subtract: assessment - assess
[Observed Error] List((m,1), (n,1), (t,1)) did not equal List((e,1), (m,1), (n,1), (t,1))
[Lost Points] 10

[Test Description] subtract: jimmy - my
[Observed Error] List((i,1), (j,1)) did not equal List((i,1), (j,1), (m,1))
[Lost Points] 10

[Test Description] sentence anagrams: Heather
[Observed Error] Set(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha")) did not equal Set(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("heather"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha"))
[Lost Points] 10


2nd_submmit:

Your overall score for this assignment is 9.69 out of 10.00

Tests that were aborted took too long too complete or crashed the
JVM. Such crashes can arise due to infinite non-terminitaing
loops or recursion (StackOverflowException) or excessive mamory
consumption (OutOfMemoryException).

[Test Description] sentence anagrams: Heather
[Observed Error] Set(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha")) did not equal Set(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("heather"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha"))
[Lost Points] 10

 */


}
