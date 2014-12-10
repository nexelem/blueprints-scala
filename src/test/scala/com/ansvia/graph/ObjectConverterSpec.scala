package com.ansvia.graph

import org.specs2.Specification
import com.tinkerpop.blueprints.impls.tg.TinkerGraphFactory
import com.tinkerpop.blueprints.Vertex
import com.ansvia.graph.BlueprintsWrapper._
import scala.collection.JavaConversions._
import org.specs2.specification.Step
import scala.language.reflectiveCalls

/**
 * Copyright (C) 2011-2012 Ansvia Inc.
 * User: robin
 * Date: 12/31/12
 * Time: 5:21 AM
 *
 */
class ObjectConverterSpec extends Specification {

    import com.ansvia.graph.testing.model._

    def is = sequential ^
        "Object cenverter should" ^
        p ^
            "convert vertex to case class #1" ! oc.vtocc1 ^
            "convert vertex to case class #2" ! oc.vtocc2 ^
            "convert vertex to case class #3" ! oc.vtocc3 ^
            "convert case class to vertex #1" ! oc.cctov1 ^
            "convert case class to vertex #3" ! oc.cctov2 ^
            "convert back from vertex to case class #1" ! oc.cbvtocc1 ^
            "convert back from vertex to case class #2" ! oc.cbvtocc2 ^
            "convert back from vertex to case class #3" ! oc.cbvtocc3 ^
            Step(oc.close()) ^
        p ^
        "DbObject inherited class should" ^
            "work with non param var get parameter" ! dboic.nonParamVarGet ^
            "work with non param var get inner variable #1" ! dboic.nonParamVarGetInner ^
            "work with non param var get inner variable #2" ! dboic.nonParamVarGetInner2 ^
            "save directly using .save()" ! dboic.saveDirectlyUsingSave ^
            "has expected field" ! dboic.hasExpectedField ^
            "get back saved field data" ! dboic.getBackSavedFieldData ^
            "deserializable" ! dboic.deserializable ^
            "has expected data in deserialized object" ! dboic.expectedDataDeserializedObj ^
            "able to get raw vertex from case class" ! dboic.getRawVertexFromCC ^
            "make relation DbObject to DbObject #1" ! dboic.makeRelDbo2Dbo ^
            "make relation DbObject to DbObject #2" ! dboic.makeRelDbo2Dbo2 ^
            "saved DbObject return true in isSaved" ! dboic.savedDboRetTrue ^
            "class contain lazy or val should not raising invocation error" ! dboic.ccContainLazyNotError ^
            "access upper variable #1" ! dboic.accessUpperVar1 ^
            "access upper variable #2" ! dboic.accessUpperVar2 ^
            "access upper-upper variable #1" ! dboic.accessUpperUpperVar1 ^
            "access upper-upper variable #2" ! dboic.accessUpperUpperVar2 ^
            "access upper-upper variable #3" ! dboic.accessUpperUpperVar3 ^
            "access lower variable via loader from upper-upper" ! dboic.accessLowVarViaLoaderFromUpperUpper ^
            "access trait variable" ! dboic.accessTraitVar ^
            "not saving non persisted var" ! dboic.noSaveNonPersistedVar ^
            "saved via __save__()" ! dboic.savedViaSaveMethod ^
            Step(dboic.close()) ^
        p ^
            "convert vertex to witbier case class #1" ! DoubleHandlingWithBeerStyles.witbierDefinedTest ^
            "convert vertex to witbier case class #2" ! DoubleHandlingWithBeerStyles.witbierStyleTest ^
            "convert vertex to witbier case class #3" ! DoubleHandlingWithBeerStyles.witbierAbvTest ^
            "convert pale ale case class to vertex" ! DoubleHandlingWithBeerStyles.classPaleAleTest ^
            "convert vertex back to pale ale case class #1" ! DoubleHandlingWithBeerStyles.paleAleDefinedTest ^
            "convert vertex back to pale ale case class #2" ! DoubleHandlingWithBeerStyles.paleAleStyleTest ^
            "convert vertex back to pale ale case class #3" ! DoubleHandlingWithBeerStyles.paleAleAbvTest ^
            "convert ris case class to vertex" ! DoubleHandlingWithBeerStyles.classPaleAleTest ^
            "convert vertex back to ris case class #1" ! DoubleHandlingWithBeerStyles.paleAleDefinedTest ^
            "convert vertex back to ris case class #2" ! DoubleHandlingWithBeerStyles.paleAleStyleTest ^
            "convert vertex back to ris case class #3" ! DoubleHandlingWithBeerStyles.paleAleAbvTest ^
            Step(DoubleHandlingWithBeerStyles.close()) ^
    end

    object oc {

        implicit val db = TinkerGraphFactory.createTinkerGraph()

        val v = db.addVertex(null)
        v.set("name", "robin")
        v.set("age", 25)
        v.set("_class_", "com.ansvia.graph.testing.model.User")

        val vtcc1 = ObjectConverter.toCC[User](v)

        val ccU = User("gondez", 35)

        val v2 = db.save(ccU)

        val vtcc2 = ObjectConverter.toCC[User](v2)

        def close(){
            db.shutdown()
        }

        def vtocc1 = vtcc1.isDefined must beTrue
        def vtocc2 = vtcc1.get.name must beEqualTo("robin")
        def vtocc3 = vtcc1.get.age must beEqualTo(25)
        def cctov1 = v2.get("_class_").isDefined must beTrue
        def cctov2 =  v2.get[String]("_class_").get must beEqualTo("com.ansvia.graph.testing.model.User")
        def cbvtocc1 = vtcc2.isDefined must beTrue
        def cbvtocc2 = vtcc2.get.name must beEqualTo("gondez")
        def cbvtocc3 = vtcc2.get.age must beEqualTo(35)


    }

    object dboic {

        implicit val db = TinkerGraphFactory.createTinkerGraph()

        val v4o = Animal("cat")
        v4o.age = 5
        v4o.kind = "mamalia"
        val v4 = v4o.save()
        val v4ob = v4.toCC[Animal].get

        val v3 = Motor("Honda").save()
        v3 --> "hit" --> v4ob


        val nemoDraft = SeaFish("yellow")
        nemoDraft.name = "nemo"
        val nemo = nemoDraft.save().toCC[SeaFish].get

        val sharkDraft = Shark("Hammer head")
        sharkDraft.name = "the killer"
        sharkDraft.lives = "Atlantica"
        sharkDraft.eatable = false
        sharkDraft.children = 3 // this should not saved
        sharkDraft.animalProtected = true // should saved using custom __save__()
        val shark = sharkDraft.save().toCC[Shark].get

        def close(){
            db.shutdown()
        }

        def nonParamVarGet = v4ob.name must beEqualTo("cat")
        def nonParamVarGetInner = v4ob.age must beEqualTo(5)
        def nonParamVarGetInner2 = v4ob.kind must beEqualTo("mamalia")

        def saveDirectlyUsingSave = v3.isInstanceOf[Vertex] must beTrue
        def hasExpectedField = v3.has("mark") must beTrue
        def getBackSavedFieldData = v3.get[String]("mark").getOrElse("mark", "esemka") must beEqualTo("Honda")
        def deserializable = v3.toCC[Motor].isDefined must beTrue
        def expectedDataDeserializedObj = v3.toCC[Motor].get.mark must beEqualTo("Honda")
        def getRawVertexFromCC = v4ob.getVertex must beEqualTo(v4)
        def makeRelDbo2Dbo = v4ob.getVertex.pipe.in("hit").headOption.isDefined must beTrue
        def makeRelDbo2Dbo2 = v4ob.getVertex.pipe.in("hit").headOption.get.toCC[Motor].get.mark must beEqualTo("Honda")
        def savedDboRetTrue = (v4o.isSaved && v4ob.isSaved) must beTrue
        def ccContainLazyNotError = ContainLazy(1).save() must not equalTo(null)
        def accessUpperVar1 = nemo.name must beEqualTo("nemo")
        def accessUpperVar2 = nemo.color must beEqualTo("yellow")
        def accessUpperUpperVar1 = shark.color must beEqualTo("blue")
        def accessUpperUpperVar2 = shark.name must beEqualTo("the killer")
        def accessUpperUpperVar3 = shark.kind must beEqualTo("Hammer head")
        def accessLowVarViaLoaderFromUpperUpper = shark.lives must beEqualTo("Atlantica")
        def accessTraitVar = shark.eatable must beFalse
        def noSaveNonPersistedVar = shark.children must beEqualTo(0)
        def savedViaSaveMethod = shark.animalProtected must beTrue

    }


    object DoubleHandlingWithBeerStyles {

        implicit val db = TinkerGraphFactory.createTinkerGraph()

        val witbierVertex = db.addVertex(null)
        witbierVertex.set("style", "witbier")
        witbierVertex.set("abv", 3.3)
        witbierVertex.set("_class_", "com.ansvia.graph.testing.model.Beer")
        val witbier = ObjectConverter.toCC[Beer](witbierVertex)

        val paleAle = Beer("pale ale", 5.5)
        val savedPaleAleVertex = db.save(paleAle)
        val savedPaleAle = ObjectConverter.toCC[Beer](savedPaleAleVertex)

        val ris = Beer("Russian Imperial Stout", 12.33333)
        val savedRisVertex = db.save(ris)
        val savedRis = ObjectConverter.toCC[Beer](savedRisVertex)

        def close(){
            db.shutdown()
        }

        def witbierDefinedTest = witbier.isDefined must beTrue
        def witbierStyleTest = witbier.get.style must beEqualTo("witbier")
        def witbierAbvTest = witbier.get.abv must beEqualTo(3.3)

        def classPaleAleTest = savedPaleAleVertex.get("_class_") must beEqualTo(Some("com.ansvia.graph.testing.model.Beer"))

        def paleAleDefinedTest = savedPaleAle.isDefined must beTrue
        def paleAleStyleTest = savedPaleAle.get.style must beEqualTo("pale ale")
        def paleAleAbvTest = savedPaleAle.get.abv must beEqualTo(5.5)

        def risDefinedTest = savedPaleAle.isDefined must beTrue
        def risStyleTest = savedPaleAle.get.style must beEqualTo("Russian Imperial Stout")
        def risAbvTest = savedPaleAle.get.abv must beEqualTo(12.33333)
    }

}
