import akka.actor.{ActorLogging, Props, ActorSystem, Actor}


trait EmailType


//our messages for our actors
case class NewsLetter(email: String, name: String)

case class UserSignedUp(email: String, user: String)

//email types
case object NewsLetterEmail extends EmailType

case object AccountSignedUpEmail extends EmailType

case class Email(from: String, to: String, subject: String, body: String, cc: String = "", bc: String = "")

object Emailer {

  private def buildMsg(name: String, msgType: EmailType) = {

    msgType match {
      case AccountSignedUpEmail => s"Hi ${name}, thanks for signing up to cool app."
      case NewsLetterEmail => "News letter content on Akka"
      case _ => ""
    }
  }

  private def buildSubject(msgType: EmailType) = {
    msgType match {
      case AccountSignedUpEmail => "Welcome"
      case NewsLetterEmail => "News Letter"
      case _ => ""
    }
  }


  def draftEmail(toAddress: String, name: String, msgType: EmailType) = {

    val body = buildMsg(name, msgType)
    val subject = buildSubject(msgType)
    Email("info@coolapp.com", toAddress, subject, body)
  }


  def sendEmail(emailMsg: Email) = {
    //todo: add smtp stuff here to actually send the email

    //todo send email here
    println(emailMsg)
  }
}


class EmailActor extends Actor with ActorLogging {
  def receive = {
    case UserSignedUp(email, name) =>
      val msg = Emailer.draftEmail(email, name, AccountSignedUpEmail)
      log.info(s"send user signed up welcome message as --> ${msg}")
      Emailer.sendEmail(msg)
    case NewsLetter(email, name) =>
      val msg = Emailer.draftEmail(email, name, NewsLetterEmail)
      log.info(s"sending news letter as --> ${msg}")
      Emailer.sendEmail(msg)
  }
}

object Main extends App {
  val system = ActorSystem("email-system")

  val emailActor = system.actorOf(Props[EmailActor], "email-actor1")

  //assuming a user has signed up as we want to notify the email service
  emailActor ! UserSignedUp("johndoe@someeail.com", "John Doe")


  def getSomeRandom1000Users = (1 to 1000).map(x => (s"email-${x}@somedomain.com", s"user-${x}"))

  getSomeRandom1000Users.foreach { x =>
    val randomEmailActor = system.actorOf(Props[EmailActor])
    val (email, name) = x
    randomEmailActor ! NewsLetter(email, name)
  }

  //last line is commented so that we can see that all our actors are being called
  //system.shutdown();
}
