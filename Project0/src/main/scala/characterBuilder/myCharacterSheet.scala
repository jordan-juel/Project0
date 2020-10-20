import org.bson.types.ObjectId

case class myCharacterSheet(_id: ObjectId, charName: String, tier: Int, attr: List[Int],
                            skills: List[Int], remainingPoints: Int, race: String,
                            archetypeName: String){

  def toMyChar(): (myChar , String) ={

    val currentChar = new myChar(charName)
    currentChar.setCharTier(tier)
    currentChar.setAllCharAttributes(attr)
    currentChar.setAllCharSkills(skills)
    currentChar.setCharRace(race)
    currentChar.setRemainingPoints(remainingPoints)

    return (currentChar, archetypeName)
  }
  def getName(): String ={
    this.charName
  }
  def getTier(): Int ={
    this.tier
  }

}


object myCharacterSheet{
  def apply(title: myChar) : myCharacterSheet = myCharacterSheet(new ObjectId(), title.getCharName(),
    title.getCharTier(), title.getCharAttributes(), title.getCharSkills(), title.getRemainingPoints()
    ,title.getCharRace(), title.getCharArchetype().getName())
}