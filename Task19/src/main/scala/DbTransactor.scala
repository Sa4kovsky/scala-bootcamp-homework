import DbConfig.{dbDriverName, dbPwd, dbUrl, dbUser}
import cats.effect.{Async, Blocker, ContextShift, Resource}
import doobie.{ExecutionContexts, Transactor}
import doobie.hikari.HikariTransactor

object DbTransactor {
  def pooled[F[_] : ContextShift : Async]: Resource[F, Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](10)
      be <- Blocker[F]
      xa <- HikariTransactor.newHikariTransactor[F](
        driverClassName = DbConfig.dbDriverName,
        url = DbConfig.dbUrl,
        user = DbConfig.dbUser,
        pass = DbConfig.dbPwd,
        connectEC = ce,
        blocker = be
      )
    } yield xa
}
