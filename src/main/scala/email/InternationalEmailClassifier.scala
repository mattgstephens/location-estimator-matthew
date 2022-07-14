package email

import parser.GenericParseCSVInternational

object InternationalEmailClassifier extends App {

  val DOMAIN_MAP_BASIC = getDomainMapBasic
  val DOMAIN_MAP_HL = getDomainMapHL

  // Recommendations off Basic CSV
  // ***---------------------------------------------------------***
  def getDomainMapBasic: Map[String, String] = {
    val country_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/email_domains.csv"
      )
    val domain_map = country_list
      .map(countryPair => countryPair.split(","))
      .map(splitPair => (splitPair(0), splitPair(1)))
      .toMap
    domain_map
  }

  def getBasicDomainCountry(email: String): Option[String] = {
    val user_domain: String = email.substring(email.lastIndexOf("."))
    // The below code is to search through the other CSV when the country is not found in the basic one!
    DOMAIN_MAP_BASIC.get(user_domain) match {
      case Some(result) => Some(result)
      case None         => Some(getHLDomainType(email))
    }
  }

  // TODO:
  // Need to fix this check --> will continue the workflow if and only if this check passes (i.e. if
  // you have a domain that is a country-code and not one that is generic.
  def identifiableDomain(email: String): Boolean = {
    getBasicDomainCountry(email) match {
      case Some(country) =>
        System.out.println("Country in identify: " + country)
        if (!country.equals("generic")) true
        else false
    }
  }

  // Recommendations off Higher Level CSV
  // ***---------------------------------------------------------***
  def getDomainMapHL: Map[String, (String, String)] = {
    val domain_information_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/trusted-email-domains.csv"
      )
    val domain_map = domain_information_list
      .map(entry => entry.split(","))
      .map(splitPair => (splitPair(0), (splitPair(1), splitPair(2))))
      .toMap
    domain_map
  }

  def getHLDomainType(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_type: String =
      DOMAIN_MAP_HL.get(user_domain).map(Tuple => Tuple._1).toString
    domain_type
  }

  def getHLDomainTLD(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_TLD: String =
      DOMAIN_MAP_HL.get(user_domain).map(Tuple => Tuple._2).toString
    domain_TLD
  }

  System.out.println(
    "---------------Now Printing the Basic CSV Functions----------------"
  )

  System.out.println(
    "---------------Get Domain----------------"
  )

  System.out.println(
    getBasicDomainCountry("matthew@brown.co.uk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getBasicDomainCountry("matthew_stephens@brown.co.zw")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getBasicDomainCountry("matthew@brown.co.za")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getBasicDomainCountry("matthew@brown.co.fr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getBasicDomainCountry("matthew@brown.co.gr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getBasicDomainCountry("matthew@brown.co.hk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Spicy one: " +
      getBasicDomainCountry("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------Identify----------------"
  )

  System.out.println(
    identifiableDomain("matthew@brown.co.uk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifiableDomain("matthew_stephens@brown.co.zw")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifiableDomain("matthew@brown.co.za")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifiableDomain("matthew@brown.co.fr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifiableDomain("matthew@brown.co.gr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    identifiableDomain("matthew@brown.co.hk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Spicy one: " +
      identifiableDomain("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------Now Printing the HL Functions----------------"
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

}
