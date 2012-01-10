package surveys

import surveys.model.Subject

trait Category {
  def contains(x: Subject): Boolean
  protected def byPrefix(xs: Set[String]): Subject => Boolean =
    x => xs map (x.code startsWith _) exists identity

  val name: String
  def title(percent: Int): String
}

trait Categorization {
  val categories: List[Category]
  val ordering: Ordering[Category] = Ordering.by(categories indexOf (_))

  def assign(x: Subject): Category = {
    categories filter (_ contains x) match {
      case x :: Nil => x
      case Nil => error("Subject %1s was not matched by any category".format(x))
      case xs => error("Subject %1s matched more than one category: %2s".format(x, xs map (_.name)))
    }
  }
}

object OneCatCategorization extends Categorization {
  private val przedmioty = new Category {
      val name = "wszystkie przedmioty"
      def contains(x: Subject) = true
      def title(percent: Int) = "%d%% spośród wszystkich przedmiotów".format(percent)
  }

  val categories = przedmioty :: Nil
}

object MathCategorization extends Categorization {
  private val fakultatywne = new Category {
      val name = "fakultatywne"
      def contains(x: Subject) = x.code startsWith "1000-13"
      def title(percent: Int) = "%d%% spośród przedmiotów fakultatywnych".format(percent)
  }

  private val kursowe = new Category {
      val name = "kursowe"
      def contains(x: Subject) = x.code startsWith "1000-11"
      def title(percent: Int) = "%d%% spośród przedmiotów kursowych".format(percent)
  }

  private val monograficzne = new Category {
      val name = "monograficzne"
      def contains(x: Subject) = x.code startsWith "1000-1M"
      def title(percent: Int) = "%d%% spośród przedmiotów monograficznych".format(percent)
  }

  val categories = kursowe :: fakultatywne :: monograficzne :: Nil
}

object CSCategorization extends Categorization {

  private val kursowe = new Category {
    val name = "kursowe"
    def contains(x: Subject) = {
      val prefs = Set("1000-21", "1000-22", "1000-23", "1000-24")
      (x.code == "1000-2L5ZP1") || (x.code == "1000-2L5ZP2") || (!(obieralne contains x) && !(staleObieralne contains x) && byPrefix(prefs)(x))
    }
    def title(percent: Int) = "%d%% spośród przedmiotów kursowych".format(percent)
  }

  private val staleObieralne = new Category {
    val name = "stałe obieralne"
    def contains(x: Subject) = 
      Set(
        "1000-234aALG", // Algorytmika
        "1000-2M09ALT", //Algorytmy tekstowe
        "1000-2M09KDW", //Kompresja danych - wprowadzenie
        "1000-234aPLO", //Programowanie w logice
        "1000-2M09SUS", //Systemy uczące się
        "1000-234aSID", //Sztuczna inteligencja i systemy doradcze
        "1000-2M03TI",  //Teoria informacji
        "1000-2M09WWK", //Weryfikacja wspomagana komputerowo
        "1000-2M09WSS", //Wnioskowanie w serwisach i systemach informatycznych
        "1000-2M03BO",  //Wstęp do biologii obliczeniowej
        "1000-2M09ZBD", //Zaawansowane bazy danych
        "1000-2M09ZSO"  //Zaawansowane systemy operacyjne
      ) contains x.code
    def title(percent: Int) = "%d%% spośród przedmiotów stałych obieralnych".format(percent)
  }

  private val obieralne = new Category {
    val name = "obieralne"
    def contains(x: Subject) = {
      val prefs = Set("1000-2M", "1000-2P")
      (!(staleObieralne contains x) && byPrefix(prefs)(x)) || (x.code == "1000-245aPLO") || (x.code == "1000-245aSID")
    }
    def title(percent: Int) = "%d%% spośród przedmiotów obieralnych".format(percent)
  }

  private val bioinf = new Category {
    val name = "bioinformatyczne"
    def contains(x: Subject) =
      Set(
        "1000-711WIN", //Wstęp do informatyki (bioinf)
        "1000-712ASD", //Algorytmy i struktury danych (bioinf)
        "1000-713PPO", //Programowanie i projektowanie obiektowe (bioinf)
        "1000-714SAD"  //Statystyczna analiza danych (bioinf)
      ) contains x.code
    def title(percent: Int) = "%d%% spośród przedmiotów na bioinformatyce".format(percent)
  }

  val categories = kursowe :: staleObieralne :: obieralne :: bioinf :: Nil
}
