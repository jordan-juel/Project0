import org.bson.types.ObjectId

class myChar(name: String) {

    var charName = name
    var attributes = List.fill(7)(1)
    var skills = List.fill(18)(0)
    var charTier = 1
    var additionalPoints = 0
    var remainingPoints = 100
    var race = "human"
    var archetype: Archetype = null
    var minAttributes = List.fill(7)(1)
    var minSkills = List.fill(18)(0)


    def getCharName(): String = {
      charName
    }

    def setCharName(newName: String): Unit = {
      this.charName = newName
    }

    def getCharTier(): Int = {
      charTier
    }

    def setCharTier(newTier: Int): Unit = {
      this.charTier = newTier
    }

    def getRemainingPoints(): Int ={
      remainingPoints
    }

    def setRemainingPoints(newPoints: Int): Unit = {
      this.remainingPoints = newPoints
    }

    def changeRemainingPoints(pointDifference: Int): Unit ={
      remainingPoints = remainingPoints + pointDifference
    }

    def getCharRace(): String = {
      race
    }

    def setCharRace(newRace: String): Unit = {
      this.race = newRace
    }

    def getCharArchetype(): Archetype = {
      archetype
    }

    def setCharArchetype(newArchetype: Archetype): Unit = {
      this.archetype = newArchetype
    }

    def getCharAttributes(): List[Int] = {
      attributes
    }

    def setCharAttribute(index: Int, value: Int): Unit = {
      attributes = attributes.patch(index,List(value),1)
    }

    def setAllCharAttributes(newAttributes:List[Int]): Unit = {
      this.attributes = newAttributes
    }

    def getCharSkills(): List[Int] = {
      skills
    }

    def setCharSkill(index: Int, value: Int): Unit = {
      skills = skills.patch(index, List(value), 1)
    }

    def setAllCharSkills(newSkills: List[Int]): Unit = {
      this.skills = newSkills
    }

    def setMinCharAttributes(newAttributes:List[Int]): Unit ={
      this.minAttributes = newAttributes
    }

    def getMinCharAttributes(): List[Int] = {
      minAttributes
    }

    def setMinCharSkills(newSkills: List[Int]): Unit ={
      this.minSkills = newSkills
    }

    def getMinCharSkills(): List[Int] = {
      minSkills
    }
}