import java.io.{FileInputStream, FileNotFoundException}
import java.io._
import java.nio.file.{Files, Paths}
import java.io.File

import org.bson.types.ObjectId
import io.circe.{Decoder, HCursor, Json, parser}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, Observable}
import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.codecs.Macros
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.model.Updates.set
import org.mongodb.scala.model.{Filters, Projections}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS, pairIntToDuration}


//TODO Remove print statements
//TODO Fix comments
//TODO Remove commented out code


object Project0{
  def main(args: Array[String]): Unit = {

    //Check for local files
    if (!Files.exists(Paths.get("./characters"))) {
      val charFolder = new File("./characters")
      charFolder.mkdir()
    }
    //Check if file exists and close program if not
    if (!Files.exists(Paths.get("w&gClassData.json"))) {
      println("Cannot find w&gClassData.json. Please place the file into the directory and restart the program")
    }
    else {

      //Parse JSON
      val archetypeData: List[Archetype] = parseJSONFile()

      println("Done parsing")

      //class Player
      var currentChar = new myChar("")
      var validOption = false
      var userInput = ""
      //User Input loop
      while (!validOption) {
        println("Would you like to create a 'new' character or 'load' an existing character?")
        userInput = scala.io.StdIn.readLine()

        userInput.toLowerCase match {
          case "new" => validOption = true
            newCharacter(currentChar, archetypeData)
          case "load" => validOption = true
            currentChar = loadCharacter(archetypeData)
          case _ => validOption = false
            println("That is not a valid command")
        }
      }

      validOption = false

      while (true) {
        println(s"Your character has points ${currentChar.getRemainingPoints()} remaining")
        userInput = scala.io.StdIn.readLine()
        //Check which command was input
        userInput.toLowerCase match {
          case "tier" => selectTier(currentChar)
          case "race" => selectRace(currentChar, archetypeData)
          case "archetype" => selectArchetype(currentChar, archetypeData)
          case "attributes" => editAttributes(currentChar)
          case "skills" => editSkills(currentChar)
          case "name" => editName(currentChar)
          case "save" => saveCharacter(currentChar)
          case "load" => currentChar = loadCharacter(archetypeData)
          case _ => println("That is not a valid command")
        }
      }
    }
  }
  def newCharacter(currentChar:myChar, archetypeData: List[Archetype]): Unit ={
    println("New Character")
    editName(currentChar)
    selectTier(currentChar)
    selectRace(currentChar, archetypeData)
  }

  def selectTier(currentChar:myChar): Unit = {
    println("Select Tier:")
    var validInput = false
    var tierChoice = -1
    while(!validInput){
      println("Input must be between 1 and 4")
      val userInput = scala.io.StdIn.readLine()
      try{
        tierChoice = userInput.toInt
      }
      catch{
        case e: NumberFormatException => println("Input is not an Int")
      }

      if(0 < tierChoice && tierChoice < 5){
        //Calculate the point difference
        val tierDiff = tierChoice - currentChar.getCharTier()

        validInput = true
        //Place tier and change points
        currentChar.setCharTier(tierChoice)
        currentChar.changeRemainingPoints(tierDiff*100)
      }
      else{
        println("Please enter a valid value")
      }
    }
  }
  def selectRace(currentChar:myChar, archetypeData: List[Archetype]): Unit = {
    println("Select Race:")

    //Create list of races
    val filteredRace = archetypeData.filter(_.getTier() <= currentChar.getCharTier())
    var filteredRaceNames = for(f <- filteredRace) yield f.getRace()
    filteredRaceNames = filteredRaceNames.distinct

    for(f <- filteredRaceNames){
      print(s"'${f}' ")
    }
    println("")

    //Take in user input
    var validInput = false
    while(!validInput){
      val userInput = scala.io.StdIn.readLine().toLowerCase()
      //Check if input is valid
      if(userInput == "back"){
        validInput = true
      }
      else if(filteredRaceNames.contains(userInput)){
        //Put input into character
        currentChar.setCharRace(userInput)
        selectArchetype(currentChar, archetypeData)
        validInput = true
      }
      else{
        println("Not a valid race")
      }
    }
  }

  def selectArchetype(currentChar:myChar, archetypeData: List[Archetype]): Unit = {
    println("Select Archetype:")

    //Create list of Archetypes from parser
    val filteredArch = archetypeData.filter(_.getRace() == currentChar.getCharRace()).filter(_.getTier()<= currentChar.getCharTier())
    val filteredArchNames =  for(f <- filteredArch) yield f.getName()
    for(f <- filteredArch){
      print(s"'${f.getName()}' ")
    }
    println("")

    //Take user input
    var validInput = false
    while(!validInput) {
      val userInput = scala.io.StdIn.readLine().toLowerCase()
      if(userInput.toLowerCase() == "back"){
        validInput = true
      }
      //Check if valid
      else if(filteredArchNames.contains(userInput)){
        //Put input into character
        val selectedArchetype = filteredArch.apply(filteredArchNames.indexOf(userInput))

        //Check if character has an archetype and refund that cost before setting new archetype
        currentChar.setRemainingPoints((currentChar.getCharTier()* 100)-selectedArchetype.getCost())

        currentChar.setCharArchetype(selectedArchetype)
        currentChar.setAllCharAttributes(selectedArchetype.getAttributes())
        currentChar.setMinCharAttributes(selectedArchetype.getAttributes())

        currentChar.setAllCharSkills(selectedArchetype.getSkills())
        currentChar.setMinCharSkills(selectedArchetype.getSkills())

        validInput = true
      }
      else{
        println("Not a valid archetype")
      }
    }
  }

  def editAttributes(currentChar:myChar): Unit = {
    println("Edit Attributes:")

    var validInput = false
    var attrIndex = -1
    while(attrIndex < 0) {
      //Take in value for which attribute
      println(s"would you like to edit 'strength': ${currentChar.getCharAttributes().apply(0)}" +
        s", 'toughness': ${currentChar.getCharAttributes().apply(1)}" +
        s", 'agility': ${currentChar.getCharAttributes().apply(2)}" +
        s", 'initiative': ${currentChar.getCharAttributes().apply(3)}" +
        s", 'willpower': ${currentChar.getCharAttributes().apply(4)}" +
        s", 'intellect': ${currentChar.getCharAttributes().apply(5)}" +
        s", or 'fellowship': ${currentChar.getCharAttributes().apply(6)}?")
      val userInput = scala.io.StdIn.readLine().toLowerCase()

      //Check if valid
      attrIndex = userInput match{
        case "strength" => 0
        case "toughness" => 1
        case "agility" => 2
        case "initiative" => 3
        case "willpower" => 4
        case "intellect" => 5
        case "fellowship" => 6
        case "back" => validInput = true
          -1
        case _ => println("That is not a valid command")
          -1
      }
    }

    var newAttrValue = -1
    while(!validInput) {
      println(s"The current value is ${currentChar.getCharAttributes().apply(attrIndex)}")
      println("What would you like to change it to?")
      //Take in value for attribute
      val userInput = scala.io.StdIn.readLine()

      try{
        newAttrValue = userInput.toInt
      }
      catch{
        case e: NumberFormatException => println("Input is not an Int")
      }


      //Look up upper bound based on race
      val maxAttrValue = currentChar.getCharRace() match{
        case "human" => List(8,8,8,8,8,8,8)
        case "adeptus astartes" => List(10,10,9,9,10,10,8)
        case "primaris astartes" => List(12,12,9,9,10,10,8)
        case "aeldari" => List(7,7,12,12,12,10,6)
        case "ork" => List(12,12,7,7,8,7,7)
      }

      //Checks if above min for archetype and below max for the race
      if(currentChar.getMinCharAttributes().apply(attrIndex) <= newAttrValue && newAttrValue <= maxAttrValue.apply(attrIndex)){

        //Calculate cost of change
        val attrCosts = List(0, 0, 4, 6, 10, 15, 20, 25, 30, 35, 40, 45, 50)
        val pointCost = calculateChangeCost(newAttrValue, currentChar.getCharAttributes().apply(attrIndex), attrCosts)

        //Put into list in myChar
        currentChar.setCharAttribute(attrIndex, newAttrValue)
        //Take or refund points
        currentChar.changeRemainingPoints(pointCost)
        validInput = true
      }
      else{
        println("Not a valid value")
        println(s"The valid range for your race+archetype is " +
          s"${currentChar.getMinCharAttributes().apply(attrIndex)} - ${maxAttrValue.apply(attrIndex)}")
      }
    }
  }

  def editSkills(currentChar:myChar): Unit = {
    println("Edit Skills:")

    println(s"would you like to edit 'athletics': ${currentChar.getCharSkills().apply(0)}" +
      s", 'awareness': ${currentChar.getCharSkills().apply(1)}" +
      s", 'ballistic skill': ${currentChar.getCharSkills().apply(2)}" +
      s", 'cunning': ${currentChar.getCharSkills().apply(3)}" +
      s", 'deception': ${currentChar.getCharSkills().apply(4)}" +
      s", 'insight': ${currentChar.getCharSkills().apply(5)}" +
      s", 'intimidation': ${currentChar.getCharSkills().apply(6)}" +
      s", 'investigation': ${currentChar.getCharSkills().apply(7)}" +
      s", 'leadership': ${currentChar.getCharSkills().apply(8)}" +
      s", 'medicae': ${currentChar.getCharSkills().apply(9)}" +
      s", 'persuation': ${currentChar.getCharSkills().apply(10)}" +
      s", 'pilot': ${currentChar.getCharSkills().apply(11)}" +
      s", 'psychic mastery': ${currentChar.getCharSkills().apply(12)}" +
      s", 'scholar': ${currentChar.getCharSkills().apply(13)}" +
      s", 'stealth':${currentChar.getCharSkills().apply(14)}" +
      s", 'survival': ${currentChar.getCharSkills().apply(15)}" +
      s", 'tech': ${currentChar.getCharSkills().apply(16)}" +
      s", or 'weapon skill': ${currentChar.getCharSkills().apply(17)}")



    var validInput = false
    var skillIndex = -1
    while(skillIndex < 0) {
      //Take in value for which skill
      val userInput = scala.io.StdIn.readLine().toLowerCase()
      //Check if valid
      skillIndex = userInput match{
        case "athletics" => 0
        case "awareness" => 1
        case "ballistic skill" => 2
        case "cunning" => 3
        case "deception" => 4
        case "insight" => 5
        case "intimidation" => 6
        case "investigation" => 7
        case "leadership" => 8
        case "medicae" => 9
        case "persuation" => 10
        case "pilot" => 11
        case "psychic mastery" => 12
        case "scholar" => 13
        case "stealth" => 14
        case "survival" => 15
        case "tech" => 16
        case "weapon skill" => 17
        case "back" => validInput = true
          -1
        case _ => println("That is not a valid command")
          -1
      }
    }

    println(s"The current value is ${currentChar.getCharSkills().apply(skillIndex)}")
    println("What would you like to change it to?")

    var newSkillValue = -1
    while(!validInput) {
      val userInput = scala.io.StdIn.readLine()

      try{
        newSkillValue = userInput.toInt
      }
      catch{
        case e: NumberFormatException => println("Input is not an Int")
      }

      //Checks if above min for archetype and below 8
      if(currentChar.getMinCharSkills().apply(skillIndex) <= newSkillValue && newSkillValue <= 8){

        //Calculate cost of change
        val skillCosts = List(0, 2, 4, 6, 8, 10, 12, 14, 16)
        val pointCost = calculateChangeCost(newSkillValue, currentChar.getCharSkills().apply(skillIndex),skillCosts)

        //Put into list in myChar
        currentChar.setCharSkill(skillIndex, newSkillValue)
        //Take or refund points
        currentChar.changeRemainingPoints(pointCost)
        validInput = true
      }
      else{
        println("Not a valid value")
        println(s"The valid range for your archetype is " +
          s"${currentChar.getMinCharSkills().apply(skillIndex)} - 8")
      }


    }
  }

  def editName(currentChar: myChar): Unit ={
    println("What is your character called?")
    val userInput = scala.io.StdIn.readLine()
    currentChar.setCharName(userInput)
  }

  def saveCharacter(currentChar: myChar): Unit = {

    var validInput = false
    while(!validInput) {
      println("Would you like to save 'local' or to the 'database'?")
      val userInput = scala.io.StdIn.readLine().toLowerCase()

      userInput match{
        case "local" => saveLocal(currentChar)
          validInput = true
        case "database" => saveToDatabase(currentChar)
          validInput = true
        case "back" => validInput = true
        case _ => println("not a valid input")
      }
    }
  }

  def saveLocal(currentChar: myChar): Unit ={
    //Create jsonString
    val jsonString = buildJsonString(currentChar)

    val file = new File(s"./characters/${currentChar.getCharName()}.json")
    val writeBuffer = new BufferedWriter(new FileWriter(file))
    writeBuffer.write(jsonString)
    writeBuffer.close()
  }
  def saveToDatabase(currentChar: myChar): Unit = {
    println("Saving")
    val characterSheetCodecProvider = Macros.createCodecProvider[myCharacterSheet]()
    val codecRegistry = fromRegistries(fromProviders(characterSheetCodecProvider), MongoClient.DEFAULT_CODEC_REGISTRY)
    val client = MongoClient()
    val db = client.getDatabase("myDatabase").withCodecRegistry(codecRegistry)
    val collection: MongoCollection[myCharacterSheet] = db.getCollection("myCollection")

    //Check if already exists
    val checkCharExists = collection.find(Filters.equal("charName", currentChar.getCharName())).first()

    try{
      //If character already exists than overwrite
      if(getResults(checkCharExists).head != null){
        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("tier", currentChar.getCharTier())
        ))

        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("attr", currentChar.getCharAttributes())
        ))

        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("skills", currentChar.getCharSkills())
        ))

        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("remainingPoints", currentChar.getRemainingPoints())
        ))

        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("race", currentChar.getCharRace())
        ))

        printResults(collection.updateOne(equal("charName", currentChar.getCharName()),
          set("archetypeName", currentChar.getCharArchetype().getName())
        ))
      }
    }
    //if no character is found create a new entry for them
    catch{
      case e: NoSuchElementException => printResults(collection.insertOne(myCharacterSheet(currentChar)))
    }
  }
  def loadCharacter(archetypeData: List[Archetype]): myChar = {
    var validInput = false
    var loadedChar = new myChar("")
    while (!validInput) {
      println("Would you like to load 'local' or from the 'database'?")
      val userInput = scala.io.StdIn.readLine().toLowerCase()

      loadedChar = userInput match {
        case "local" => validInput = true
          return loadLocal(archetypeData)
        case "database" => validInput = true
          return loadFromDatabase(archetypeData)
        case "back" => validInput = true
          return null
        case _ => println("not a valid input")
          return null
      }
    }
    return loadedChar
  }
  def loadLocal(archetypeData: List[Archetype]): myChar ={

    var validInput = false

    while (!validInput) {

      val location = new File("./characters/")
      //Show options
      println(location.listFiles.toList)

      val userInput = scala.io.StdIn.readLine()

      try {
        val fis = new FileInputStream(s"${location}/${userInput}.json")

        val parseResult = parser.parse(scala.io.Source.fromInputStream(fis).mkString)

        parseResult match {
          case Left(failure) => println("The JSON input could not be parsed")
            null
          case Right(json) => validInput = true
            return pullLocalCharacterData(parseResult.getOrElse(Json.Null),archetypeData)
        }

      }
      catch {
        case e: FileNotFoundException => println("File not found")
          null
      }

    }

    return null

  }

  def loadFromDatabase(archetypeData: List[Archetype]): myChar = {
    //TODO fix database connection
    println("Load")
    val characterSheetCodecProvider = Macros.createCodecProvider[myCharacterSheet]()
    val codecRegistry = fromRegistries(fromProviders(characterSheetCodecProvider), MongoClient.DEFAULT_CODEC_REGISTRY)
    val client = MongoClient()
    val db = client.getDatabase("myDatabase").withCodecRegistry(codecRegistry)
    val collection: MongoCollection[myCharacterSheet] = db.getCollection("myCollection")

    var characterFromDatabase: myCharacterSheet = null

    //Display options
    printResults(collection.find())

    println("Which Character")
    //Take user input for character
    var validInput = false
    while(!validInput) {
      //Take in value for which skill
      val userInput = scala.io.StdIn.readLine()

      val someDocs = collection.find(Filters.equal("charName", userInput)).first()

      try{
        if(getResults(someDocs).head != null){
          characterFromDatabase = getResults(someDocs).head
          validInput = true
        }
      }
      catch{
        case e: NoSuchElementException => println("Not a valid character")
      }

    }

    //Set up data in character object
    val tupleParse = characterFromDatabase.toMyChar()

    val loadedChar = tupleParse._1
    val archetypeName = tupleParse._2

    //Look up archetype from name
    val charArchetype = archetypeData.filter(_.getName() == archetypeName)

    loadedChar.setCharArchetype(charArchetype(0))

    return loadedChar
  }

  def getResults[T](obs: Observable[T]): Seq[T] = {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit = {
    getResults(obs).foreach(println(_))
  }

  //TODO Fix path to JSON
  def parseJSONFile(): List[Archetype] = {
    try {
      val fis = new FileInputStream("./w&gClassData.json")

      val parseResult = parser.parse(scala.io.Source.fromInputStream(fis).mkString)

      parseResult match {
        case Left(failure) => println("The JSON input could not be parsed")
          null
        case Right(json) => pullData(parseResult.getOrElse(Json.Null))
      }

    }
    catch {
      case e: FileNotFoundException => println("File not found")
        null
    }
  }
  def pullData(doc: Json): List[Archetype] = {
    val cursor: HCursor = doc.hcursor

    var currentArchetype = cursor.downField("archetypes").downArray

    var archetypes = List[Archetype]()

    while(currentArchetype.succeeded){

      val pulledRace = trimJson(currentArchetype.downField("race").as[String].toString)
      val pulledCost = trimJson(currentArchetype.downField("cost").as[Int].toString).toInt
      val pulledName = trimJson(currentArchetype.downField("name").as[String].toString)
      val pulledTier = trimJson(currentArchetype.downField("tier").as[Int].toString).toInt

      var pulledAttributes = List[Int]()

      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("fellowship").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("intellect").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("willpower").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("initiative").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("agility").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("toughness").as[Int].toString).toInt
      pulledAttributes ::= trimJson(currentArchetype.downField("attributes").downField("strength").as[Int].toString).toInt

      var pulledSkills = List[Int]()
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("weapon skill").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("tech").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("survival").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("stealth").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("scholar").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("psychic mastery").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("pilot").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("persuation").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("medicae").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("leadership").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("investigation").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("intimidation").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("insight").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("deception").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("cunning").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("ballistic skill").as[Int].toString).toInt
      pulledSkills ::=trimJson(currentArchetype.downField("skills").downField("awareness").as[Int].toString).toInt
      pulledSkills ::= trimJson(currentArchetype.downField("skills").downField("athletics").as[Int].toString).toInt

      currentArchetype = currentArchetype.right

      archetypes ::= new Archetype(pulledName,pulledRace,pulledTier,pulledCost,pulledAttributes,pulledSkills)

    }
    archetypes
  }

  def pullLocalCharacterData(doc: Json, archetypeData: List[Archetype]): myChar = {

    val cursor: HCursor = doc.hcursor

    val charName = trimJson(cursor.downField("name").as[String].toString)
    val charRace = trimJson(cursor.downField("race").as[String].toString)
    val charTier = trimJson(cursor.downField("tier").as[Int].toString).toInt
    val charRemainingPoints = trimJson(cursor.downField("remainingPoints").as[Int].toString).toInt
    val archetypeName = trimJson(cursor.downField("archetype").as[String].toString)

    var charAttributes = List[Int]()

    charAttributes ::= trimJson(cursor.downField("attributes").downField("fellowship").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("intellect").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("willpower").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("initiative").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("agility").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("toughness").as[Int].toString).toInt
    charAttributes ::= trimJson(cursor.downField("attributes").downField("strength").as[Int].toString).toInt

    var charSkills = List[Int]()
    charSkills ::= trimJson(cursor.downField("skills").downField("weapon skill").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("tech").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("survival").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("stealth").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("scholar").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("psychic mastery").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("pilot").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("persuation").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("medicae").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("leadership").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("investigation").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("intimidation").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("insight").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("deception").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("cunning").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("ballistic skill").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("awareness").as[Int].toString).toInt
    charSkills ::= trimJson(cursor.downField("skills").downField("athletics").as[Int].toString).toInt

    val loadedChar = new myChar(charName)
    loadedChar.setCharRace(charRace)
    loadedChar.setCharTier(charTier)

    val charArchetype = archetypeData.filter(_.getName() == archetypeName)
    loadedChar.setCharArchetype(charArchetype.head)

    loadedChar.setRemainingPoints(charRemainingPoints)

    loadedChar.setAllCharAttributes(charAttributes)
    loadedChar.setAllCharSkills(charSkills)
    return loadedChar
  }


  def trimJson(input:String): String ={
    var output = input.dropRight(1)
    output = output.drop(6)
    output
  }

  def buildJsonString(currentChar:myChar): String ={
    val jsonString = (s"{${'"'}name${'"'}:${'"'}${currentChar.getCharName()}${'"'}," +
      s"${'"'}race${'"'}:${'"'}${currentChar.getCharRace()}${'"'}," +
      s"${'"'}tier${'"'}:${currentChar.getCharTier()}," +
      s"${'"'}remainingPoints${'"'}:${currentChar.getRemainingPoints()}," +
      s"${'"'}archetype${'"'}:${'"'}${currentChar.getCharArchetype().getName()}${'"'}," +
      s"${'"'}attributes${'"'}:{" +
      s"${'"'}strength${'"'}:${currentChar.getCharAttributes()(0)}," +
      s"${'"'}toughness${'"'}:${currentChar.getCharAttributes()(1)}," +
      s"${'"'}agility${'"'}:${currentChar.getCharAttributes()(2)}," +
      s"${'"'}initiative${'"'}:${currentChar.getCharAttributes()(3)}," +
      s"${'"'}willpower${'"'}:${currentChar.getCharAttributes()(4)}," +
      s"${'"'}intellect${'"'}:${currentChar.getCharAttributes()(5)}," +
      s"${'"'}fellowship${'"'}:${currentChar.getCharAttributes()(6)}" +
      s"}," +
      s"${'"'}skills${'"'}:{" +
      s"${'"'}athletics${'"'}:${currentChar.getCharSkills()(0)}," +
      s"${'"'}awareness${'"'}:${currentChar.getCharSkills()(1)}," +
      s"${'"'}ballistic skill${'"'}:${currentChar.getCharSkills()(2)}," +
      s"${'"'}cunning${'"'}:${currentChar.getCharSkills()(3)}," +
      s"${'"'}deception${'"'}:${currentChar.getCharSkills()(4)}," +
      s"${'"'}insight${'"'}:${currentChar.getCharSkills()(5)}," +
      s"${'"'}intimidation${'"'}:${currentChar.getCharSkills()(6)}," +
      s"${'"'}investigation${'"'}:${currentChar.getCharSkills()(7)}," +
      s"${'"'}leadership${'"'}:${currentChar.getCharSkills()(8)}," +
      s"${'"'}medicae${'"'}:${currentChar.getCharSkills()(9)}," +
      s"${'"'}persuation${'"'}:${currentChar.getCharSkills()(10)}," +
      s"${'"'}pilot${'"'}:${currentChar.getCharSkills()(11)}," +
      s"${'"'}psychic mastery${'"'}:${currentChar.getCharSkills()(12)}," +
      s"${'"'}scholar${'"'}:${currentChar.getCharSkills()(13)}," +
      s"${'"'}stealth${'"'}:${currentChar.getCharSkills()(14)}," +
      s"${'"'}survival${'"'}:${currentChar.getCharSkills()(15)}," +
      s"${'"'}tech${'"'}:${currentChar.getCharSkills()(16)}," +
      s"${'"'}weapon skill${'"'}:${currentChar.getCharSkills()(17)}" +
      "}}")
    return jsonString
  }

  def calculateChangeCost(newValue: Int, currentValue:Int, costs: List[Int]):Int ={
    var changeCost = 0
    var newList = List[Int]()
    //drop front and drop back

    if(newValue < currentValue) {
      //Get rid of front stuff
      newList = costs.take(currentValue+1)
      newList = newList.drop(newValue+1)
      changeCost = newList.sum
    }
    else{
      //Get rid of front stuff
      newList = costs.take(newValue+1)
      newList = newList.drop(currentValue+1)
      changeCost = newList.sum * (-1)
    }
    return changeCost
  }
}