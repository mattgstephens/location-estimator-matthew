package email

import parser.GenericParseCSVInternational

object InternationalEmailClassifier extends App {

  val DOMAIN_MAP_BASIC: Map[String, String] = getDomainMapBasic
  val DOMAIN_MAP_HL: Map[String, (String, String)] = getDomainMapHL
  val COUNTRIES_LIST: List[String] =
    GenericParseCSVInternational.parseCSVInternational(
      "src/main/resources/countries_google.csv"
    )

  // Process Basic CSV
  // ***---------------------------------------------------------***
  def getDomainMapBasic: Map[String, String] = {
    GenericParseCSVInternational
      .parseCSVInternational(
        "src/main/resources/basic_email_domains.csv"
      )
      .map(countryPair => countryPair.split(","))
      .map(splitPair =>
        (Seq(splitPair).lift(0).toString, Seq(splitPair).lift(1).toString)
      )
      .toMap
  }

  // Process Trusted CSV
  // ***---------------------------------------------------------***
  def getDomainMapHL: Map[String, (String, String)] = {
    GenericParseCSVInternational
      .parseCSVInternational(
        "src/main/resources/HL_email_domains.csv"
      )
      .map(entry => entry.split(","))
      .map(splitTriple =>
        (
          Seq(splitTriple).lift(0).toString,
          (Seq(splitTriple).lift(1).toString, Seq(splitTriple).lift(2).toString)
        )
      )
      .toMap
  }

  // ***---------------------------------------------------------***
  // Begin Work Flow
  // ***---------------------------------------------------------***

  // TODO:
  // Need to fix this check --> will continue the workflow if and only if this check passes (i.e. if
  // you have a domain that is a country-code and not one that is generic.
  def identifyCountryFromDomain(email: String): Option[String] = {
    getDomain(email) match {
      case Some(country) =>
        System.out.println("Country in identify: " + country)
        if (!country.equals("generic") && !country.equals("country-code")) {
          System.out.println("What are you doing here?")
          Some(country)
        } else if (
          country.equals("country-code") || country.equals("generic")
        ) {
          // Add logic here to sort through the TLD domains!
          System.out.println("Made it here!!!")
          // TODO: This needs to return an Option[String]
          // GENERATE NGRAMS
          val nGramTLD = generateNGrams(getHLDomainTLD(email), 4)
          // COMPARE THE NGRAMS TO THE TRUSTED EMAIL DOMAIN
          analyze_HLDomainTLD(nGramTLD)
        } else Some(country)
      case None => None
    }
  }

  //Input: "South African Airways"
  //Input: "Gothic South African Airways"
  //Output: ["South", "South African", "South African Airways", "African", "African Airways"]
  def generateNGrams(TLD: String, n: Int): IndexedSeq[List[String]] = {
    val words: Array[String] = TLD.split("\\s+")
    val ngrams = (for (i <- 1 to n)
      yield words.sliding(i).map(group => group.toList)).flatten
    System.out.println("Look at these lovely ngrams " + ngrams)
    ngrams
  }

  def analyze_HLDomainTLD(
      nGramTLD: IndexedSeq[List[String]]
  ): Option[String] = {
    val countriesMap = COUNTRIES_LIST.map(country => (country, country)).toMap
    val filteredNGram = nGramTLD.map(word_list =>
      word_list
        .filter(words => COUNTRIES_LIST.contains(words))
        .map(word => countriesMap.get(word))
    )
    val countryWords = filteredNGram.flatten
    if (countryWords.size == 1) {
      countryWords.headOption.flatten
    } else if (countryWords.isEmpty) {
      None
    } else {
      val sizeMap =
        countryWords.map(country => (country.toList.size, country)).toMap
      val maxKey = sizeMap.keys.toSeq.max
      sizeMap.get(maxKey).flatten
    }
  }

  def getDomain(email: String): Option[String] = {
    // The below code is to search through the other CSV when the country is not found in the basic one!
    val user_domain: String = email.substring(email.lastIndexOf("."))
    // You have to check that there is a domain type to see if the
    // domain is even in the other CSV file! If not, then there is no
    // point in even checking through the domain TLD for the various ngrams.
    DOMAIN_MAP_BASIC.get(user_domain) match {
      case Some(result) => Some(result)
      case None         => Some(getHLDomainType(email))
    }
  }

  def getHLDomainType(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_type: String =
      DOMAIN_MAP_HL
        .get(user_domain)
        .map(Tuple => Tuple._1)
        .getOrElse("")
    System.out.println("Domain_Type: " + domain_type)
    domain_type
  }

  def getHLDomainTLD(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_TLD: String =
      DOMAIN_MAP_HL
        .get(user_domain)
        .map(Tuple => Tuple._2)
        .getOrElse("")
    domain_TLD
  }

  System.out.println(
    "---------------TESTING Identify----------------"
  )

  System.out.println(
    identifyCountryFromDomain("matthew@brown.co.uk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifyCountryFromDomain("matthew_stephens@brown.co.zw")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifyCountryFromDomain("matthew@brown.co.za")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifyCountryFromDomain("matthew@brown.co.fr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifyCountryFromDomain("matthew@brown.co.gr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifyCountryFromDomain("matthew@brown.co.hk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Spicy one: " +
      identifyCountryFromDomain("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------TESTING getDomain----------------"
  )

  System.out.println(
    getDomain("matthew@brown.co.uk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getDomain("matthew_stephens@brown.co.zw")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getDomain("matthew@brown.co.za")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getDomain("matthew@brown.co.fr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getDomain("matthew@brown.co.gr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getDomain("matthew@brown.co.hk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Spicy one: " +
      getDomain("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------TESTING Get Domain Type and TLD----------------"
  )

  System.out.println("Domain type: " + getHLDomainType("matthew@brown.co.za"))
  System.out.println("TLD for domain: " + getHLDomainTLD("matthew@brown.co.za"))
  System.out.println("Domain type: " + getHLDomainType("matthew@brown.co.zw"))
  System.out.println("TLD for domain: " + getHLDomainTLD("matthew@brown.co.zw"))
  System.out.println("Domain type: " + getHLDomainType("matthew@brown.co.td"))
  System.out.println("TLD for domain: " + getHLDomainTLD("matthew@brown.co.td"))
  System.out.println(
    "Domain type: " + getHLDomainType("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------TESTING ngram algorithm----------------"
  )

  System.out.println(
    "Testing the n-gram algorithm now: " + identifyCountryFromDomain(
      "matthew@brown.台湾"
    )
  )

  System.out.println(
    "Testing the n-gram algorithm now: " + identifyCountryFromDomain(
      "matthew@brown.台湾"
    )
  )

  System.out.println(
    "Testing the n-gram algorithm now: " + identifyCountryFromDomain(
      "matthew@brown.star"
    )
  )

  System.out.println(
    "NGRAM TEST RESULT 4 steps: " + generateNGrams("This is an ngram test", 4)
  )

  System.out.println(
    "NGRAM TEST RESULT 5 steps: " + generateNGrams("This is an ngram test", 5)
  )

}

object PlayPlayGround extends App {

  val aEmailWithoutFullStop = "matt@gmailcozw"
  val aEmailWithFullStop = "matt@gmail.co.zw"

  def checkForFullStop(email: String): Boolean = {
    if (email.contains(".")) true
    else false
  }

  System.out.println(
    "Test without full stop: " + checkForFullStop(aEmailWithoutFullStop)
  )
  System.out.println(
    "Test with full stop: " + checkForFullStop(aEmailWithFullStop)
  )

}
