package github

import jsonpicklers._, Parsers._

import java.text.SimpleDateFormat
import java.util.Date
import java.net.URI

import util.control.Exception.allCatch

import org.json4s.JsonAST.{JObject, JValue}

import org.scalatest.FunSuite

class GithubExample extends FunSuite with Resources {

  def date = string.flatMap{ s =>
    getOrElse(
      allCatch.opt((new SimpleDateFormat("yyyy-MM-dd'T'hh:MM:ss'Z'")).parse(s)),
      "expected ISO-8601 date")
  }

  def uri = string.flatMap{ s =>
    getOrElse(
      allCatch.opt(new URI(s)),
      "expected URL")
  }

  case class Event( id:String,
                    `type`:String,
                    `public`:Boolean,
                    payload:JValue,
                    createdAt:Date,
                    repo:Repo,
                    actor:User,
                    org:User)

  case class Repo(id:Int, name:String, url:String)
  case class User(id:Int, login:String, url:String, avatar_url:String, gravatar_id:String)

  case class ChangeStatus(deletions:Int, additions:Int, total:Int)

  case class File(size:Int, filename:String, rawUrl:URI)

  case class Fork(user:User, url:URI, createdAt:Date)

  case class History(url:URI, version:String, user:User, changeStatus:ChangeStatus, commtedAt:Date)

  case class Gist(url:URI,
                  id:String,
                  description:String,
                  `public`:Boolean,
                  user:User,
                  files:Map[String, File],
                  comments:Int,
                  commentsUrl:URI,
                  htmlUrl:URI,
                  gitPullUrl:URI,
                  gitPushUrl:String,
                  createdAt:Date,
                  forks:List[Fork],
                  history:List[History])

  val file =
    ("size" :: int) ~
    ("filename" :: string) ~
    ("raw_url" :: uri) ^^ File

  val changeStatus =
    ("deletions" :: int) ~
    ("additions" :: int) ~
    ("total" :: int) ^^ ChangeStatus

  val user =
    ("id" :: int) ~
      ("login" :: string) ~
      ("url" :: string) ~
      ("avatar_url" :: string) ~
      ("gravatar_id" :: string) ^^ User

  val history =
    ("url" :: uri) ~
    ("version" :: string) ~
    ("user" :: user) ~
    ("change_status" :: changeStatus) ~
    ("committed_at" :: date) ^^ History

  val fork =
    ("user" :: user) ~
      ("url" :: uri) ~
      ("created_at" :: date) ^^ Fork

  val event = array(
    ("id" :: string) ~
      ("type" :: string) ~
      ("public" :: boolean) ~
      ("payload" :: jvalue) ~
      ("created_at" :: date) ~
      ("repo" ::
        ("id" :: int) ~
          ("name" :: string) ~
          ("url" :: string) ^^ Repo) ~
      ("actor" :: user) ~
      ("org" :: user) ^^ Event)

  val gist =
    ("url" :: uri) ~
    ("id" :: string)~
    ("description" :: string) ~
    ("public" :: boolean) ~
    ("user" :: user) ~
    ("files" ::
      (* :: file)) ~
    ("comments" :: int) ~
    ("comments_url" :: uri) ~
    ("html_url" :: uri) ~
    ("git_pull_url" :: uri) ~
    ("git_push_url" :: string) ~
    ("created_at" :: date) ~
    ("forks" :: array(fork)) ~
    ("history" :: array(history)) ^^ Gist

  test("event"){
    val octocat = User(1, "octocat", "https://api.github.com/users/octocat", "https://github.com/images/error/octocat_happy.gif", "somehexcode")
    val repo = Repo(3, "octocat/Hello-World", "https://api.github.com/repos/octocat/Hello-World")

    assert(event(source("github/events.json")).get ===
      List(Event("12345", "Event", true, JObject(Nil), new Date(1360166427000L),
        repo,
        octocat,
        octocat)))
  }

  test("gist"){
    val octocat = User(1,"octocat","https://api.github.com/users/octocat","https://github.com/images/error/octocat_happy.gif","somehexcode")

    assert(gist(source("github/gist.json")).get ===
      Gist(
        new URI("https://api.github.com/gists/774dd83dad7e6ca91149"),
        "1",
        "description of gist",
        true,
        octocat,
        Map("ring.erl" -> File(932,"ring.erl", new URI("https://gist.github.com/raw/365370/8c4d2d43d178df44f4c03a7f2ac0ff512853564e/ring.erl"))),
        0,
        new URI("https://api.github.com/gists/5c346d9bd9e23e126132/comments/"),
        new URI("https://gist.github.com/1"),new URI("git://gist.github.com/1.git"),
        "git@gist.github.com:1.git",
        new Date(1300064415000L),
        List(Fork(octocat,new URI("https://api.github.com/gists/d31d7e199d6ea4a3d9cb"),new Date(1292338849000L))),
        List(History(new URI("https://api.github.com/gists/f8c21d394c87e382a2aa"),"57a7f021a713b1c5a6a199b54cc514735d2d462f",octocat,ChangeStatus(0,180,180),new Date(1300064415000L)))))
  }
}
