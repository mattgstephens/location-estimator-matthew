package email

import parser.GenericParseCSVInternational

object InternationalEmailClassifier extends App {

  // Recommendations off Basic CSV
  // ***---------------------------------------------------------***
  def getCountryRecommendationBasicDomains(email: String): Option[String] = {
    // Change to correct file path!
    // Will probably have to move the next two lines into the generic parser, and take in the list
    // of country phone numbers as a second argument to getRecommendation!
    val country_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/email_domains.csv"
      )
    val domain_map = country_list
      .map(countryPair => countryPair.split(","))
      .map(splitPair => (splitPair(0), splitPair(1)))
      .toMap
    // Get the domain after the last full stop to make it searchable.
    val user_domain: String = email.substring(email.lastIndexOf("."))
    val result = domain_map.get(user_domain)
    // The below code is to search through the other CSV when the country is not found in the basic one!
//    domain_map.get(user_domain) match {
//      case Some(result) => result
//      case None => getDomainType(email)
//    }
    result
  }

  // Recommendations off Higher Level CSV
  // ***---------------------------------------------------------***
  def getDomainInformation: Map[String, (String, String)] = {
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

  def getDomainType(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_type: String =
      getDomainInformation.get(user_domain).map(Tuple => Tuple._1).toString
    domain_type
  }

  def getTLDType(email: String): String = {
    val user_domain = email.substring(email.lastIndexOf("."))
    val domain_TLD: String =
      getDomainInformation.get(user_domain).map(Tuple => Tuple._2).toString
    domain_TLD
  }

  System.out.println(
    getCountryRecommendationBasicDomains("matthew@brown.co.uk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getCountryRecommendationBasicDomains("matthew_stephens@brown.co.zw")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getCountryRecommendationBasicDomains("matthew@brown.co.za")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getCountryRecommendationBasicDomains("matthew@brown.co.fr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getCountryRecommendationBasicDomains("matthew@brown.co.gr")
  )
  System.out.println("-------------------------------")
  System.out.println(
    getCountryRecommendationBasicDomains("matthew@brown.co.hk")
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Spicy one: " +
      getCountryRecommendationBasicDomains("matthew@brown.co.telefonica")
  )

  System.out.println(
    "---------------Now Printing the New Functions----------------"
  )

  System.out.println("Domain type: " + getDomainType("matthew@brown.co.za"))
  System.out.println("TLD for domain: " + getTLDType("matthew@brown.co.za"))
  System.out.println("Domain type: " + getDomainType("matthew@brown.co.zw"))
  System.out.println("TLD for domain: " + getTLDType("matthew@brown.co.zw"))
  System.out.println("Domain type: " + getDomainType("matthew@brown.co.td"))
  System.out.println("TLD for domain: " + getTLDType("matthew@brown.co.td"))
  System.out.println(
    "Domain type: " + getDomainType("matthew@brown.co.telefonica")
  )

}
