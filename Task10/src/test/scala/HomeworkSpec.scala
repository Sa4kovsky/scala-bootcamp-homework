import java.time.{LocalDate, ZonedDateTime}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.parser._
import io.circe._
import io.circe.generic.extras._
import io.circe.generic.semiauto._
import cats.syntax.either._
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.util.Try

class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }
}

object HomeworkSpec {
  final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)
  final case class TeamBoxScore(totals: TeamTotals)
  final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
  final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  final case class BoxScore(basicGameData: Game, previousMatchup: PrevMatchup, stats: Option[GameStats])
  final case class JustScore(score: String)
  final case class TeamStats(linescore: List[JustScore], loss: String, score: String, teamId: String, triCode: String)
  final case class GameDuration(hours: String, minutes: String)
  final case class Arena(city: String, country: String, isDomestic: Boolean, name: String, stateAbbr: String)
  final case class Game(arena: Arena, attendance: String, endTimeUTC: Option[ZonedDateTime], gameDuration: GameDuration,
                        gameId: String, gameUrlCode: String, hTeam: TeamStats, isBuzzerBeater: Boolean,
                        startTimeUTC: ZonedDateTime, vTeam: TeamStats)
  final case class Scoreboard(games: List[Game], numGames: Int)

  object ModelDecoders {
    val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    implicit val dateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap[LocalDate](str => {
      Either.catchNonFatal(LocalDate.parse(str, formatter)).leftMap(_.getMessage)
    })

    //implicit val teamTotalsDecoder: Decoder[TeamTotals] = Decoder.forProduct3("assists", "full_timeout_remaining", "plusMinus")(TeamTotals.apply)
    implicit val teamTotalsDecoder: Decoder[TeamTotals] = hcursor => {
      for {
        assists <- hcursor.downField("assists").as[String]
        fullTimeoutRemaining <- hcursor.downField("full_timeout_remaining").as[String]
        plusMinus <- hcursor.downField("plusMinus").as[String]
      } yield TeamTotals(assists, fullTimeoutRemaining, plusMinus)
    }

    implicit val teamBoxScoreDecoder: Decoder[TeamBoxScore] = deriveDecoder[TeamBoxScore]
    implicit val gameStatsDecoder: Decoder[GameStats] = deriveDecoder[GameStats]
    implicit val prevMatchupDecoder: Decoder[PrevMatchup] = deriveDecoder[PrevMatchup]
    implicit val boxScoreDecoder: Decoder[BoxScore] = deriveDecoder[BoxScore]
    implicit val justScoreDecoder: Decoder[JustScore] = deriveDecoder[JustScore]
    implicit val teamStatsDecoder: Decoder[TeamStats] = deriveDecoder[TeamStats]
    implicit val gameDurationDecoder: Decoder[GameDuration] = deriveDecoder[GameDuration]
    implicit val arenaDecoder: Decoder[Arena] = deriveDecoder[Arena]
    implicit val gameDecoder: Decoder[Game] = deriveDecoder[Game]
    implicit val scoreboardDecoder: Decoder[Scoreboard] = deriveDecoder[Scoreboard]
  }

  import ModelDecoders._

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val jsonString = Try {
      val src = Source.fromResource(s"scoreboard_$dateString.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
      Http(url).asString.body
    }
    decode[Scoreboard](jsonString)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val jsonString = Try {
      val src = Source.fromResource(s"${gameId}_boxscore.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
      Http(url).asString.body
    }
    decode[BoxScore](jsonString)
  }
}