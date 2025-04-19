
import scala.io.Source
import scala.collection.mutable._
import scala.util.control.Breaks._

object Main {
  case class Parsed(units:Int,hp:Int,damage:Int,atype:String,init:Int,imm:List[String],weak:List[String],army:Int)
  class Group(var units:Int,val hp:Int,var damage:Int,val atype:String,val init:Int,val imm:List[String],val weak:List[String]){
    var target:Group = null
    var attacker:Group = null
    def effectivePower = units.toLong*damage
    def damageTo(e:Group) = if(e.imm.contains(atype))0 else if(e.weak.contains(atype))effectivePower*2 else effectivePower
  }

  def parse(lines:List[String]) = {
    val header = """(.*):""".r
    val groupR = """(\d+) units each with (\d+) hit points(.*) with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
    val immR = "immune to ([^;)]+)".r
    val weakR = "weak to ([^;)]+)".r
    var army=0
    lines.flatMap{
      case header(n) => army = if(n=="Immune System")1 else 2; None
      case groupR(u,h,e,d,t,i) =>
        val extra = e
        val imm = immR.findFirstMatchIn(extra).map(_.group(1).split(", ").toList).getOrElse(Nil)
        val weak= weakR.findFirstMatchIn(extra).map(_.group(1).split(", ").toList).getOrElse(Nil)
        Some(Parsed(u.toInt,h.toInt,d.toInt,t, i.toInt,imm,weak,army))
      case _ => None
    }
  }

  def reset(parsed:List[Parsed],boost:Int) = {
    val armies = Array(ArrayBuffer.empty[Group],ArrayBuffer.empty[Group],ArrayBuffer.empty[Group])
    val init = ArrayBuffer.empty[Group]
    for(p<-parsed){
      val g = new Group(p.units,p.hp,p.damage + (if(p.army==1)boost else 0),p.atype,p.init,p.imm,p.weak)
      armies(p.army) += g
      init += g
    }
    (armies,init)
  }

  def totalUnits(armies:Array[ArrayBuffer[Group]]) = armies(1).map(_.units).sum + armies(2).map(_.units).sum

  def findTargets(armies:Array[ArrayBuffer[Group]]) = {
    for(a<-1 to 2){
      val opp = armies(3-a)
      armies(a).sortBy(g=>(-g.effectivePower,-g.init)).foreach{g=>
        if(g.units>0){
          val cand = opp.filter(e=>e.units>0 && e.attacker==null).map(e=>(g.damageTo(e),e.effectivePower,e.init,e)).filter(_._1>0)
          if(cand.nonEmpty){
            val (_,_,_,best) = cand.maxBy{case (d,ep,ini,_)=> (d,ep,ini)}
            g.target = best
            best.attacker = g
          }
        }
      }
    }
  }

  def attack(init:ArrayBuffer[Group]) = {
    init.sortBy(- _.init).foreach{g=>
      if(g.units>0 && g.target!=null && g.target.units>0){
        val dmg = g.damageTo(g.target)
        val killed = (dmg / g.target.hp).toInt
        g.target.units = math.max(0,g.target.units - killed)
      }
    }
    init.foreach{g=> g.target = null; g.attacker = null}
  }

  def clean(armies:Array[ArrayBuffer[Group]],init:ArrayBuffer[Group]) = {
    for(a<-1 to 2) armies(a) --= armies(a).filter(_.units<=0)
    init --= init.filter(_.units<=0)
  }

  def main(args:Array[String]):Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val parsed = parse(lines)
    var boost=0
    breakable{
      while(true){
        val (armies,init) = reset(parsed,boost)
        var stalemate=false
        breakable{
          while(armies(1).nonEmpty && armies(2).nonEmpty){
            val before = totalUnits(armies)
            findTargets(armies)
            attack(init)
            val after = totalUnits(armies)
            if(after==before){ stalemate=true; break }
            clean(armies,init)
          }
        }
        if(!stalemate && armies(1).nonEmpty && armies(2).isEmpty){
          println(totalUnits(armies))
          break
        }
        boost+=1
      }
    }
  }
}
