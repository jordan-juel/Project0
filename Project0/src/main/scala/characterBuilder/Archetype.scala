

class Archetype(name: String, race: String, tier: Int, cost:Int, attributes: List[Int], skills:List[Int]) {

  def getName(): String = {
    name
  }

  def getRace(): String = {
    race
  }

  def getTier(): Int = {
    tier
  }

  def getCost(): Int = {
    cost
  }

  def getAttributes(): List[Int] = {
    attributes
  }

  def getSkills(): List[Int] = {
    skills
  }

}
