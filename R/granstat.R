granstat <-
  function(
    x,
    phiSize = FALSE,
    web_interface = FALSE,
    statistic = "all",
    modes = FALSE,
    FromLargetoSmall = TRUE
  ) {
    if (web_interface == TRUE) {
      .G2Sd_web()
    }

    if (web_interface == FALSE) {
      x <- .grancompat(x)

      if (any(as.numeric(row.names(x)) == 0)) {
        meshmin <- 1
      } else {
        meshmin <- min(as.numeric(row.names(x)))
      }

      statistic <- match.arg(
        statistic,
        c("arithmetic", "geometric", "folk.ward", "all")
      )

      if (phiSize) {
        phi = as.numeric(row.names(x))
        row.names(x) <- .phi2um(phi)
      }

      if (modes == TRUE) {
        .mode.sedim(x)
      }

      index = .index.sedim(x, decreasing = FromLargetoSmall)
      texture = .texture.sedim(x)
      descript = .sedim.descript(x)

      if (statistic == "arithmetic") {
        arith = .moment.arith(x)
        result <- list(
          stat = arith,
          index = index,
          sedim = list(
            texture = texture,
            descript = descript
          )
        )
      }

      if (statistic == "geometric") {
        geom = .moment.geom(x)
        result <- list(
          stat = geom,
          index = index,
          sedim = list(
            texture = texture,
            descript = descript
          )
        )
      }

      if (statistic == "folk.ward") {
        fowa = .fowa.stat(x, decreasing = FromLargetoSmall)
        result <- list(
          stat = fowa,
          index = index,
          sedim = list(
            texture = texture,
            descript = descript
          )
        )
      }

      if (statistic == "all") {
        arith = .moment.arith(x)
        geom = .moment.geom(x)
        fowa = .fowa.stat(x, decreasing = FromLargetoSmall)
        result <- list(
          stat = list(
            arith = arith,
            geom = geom,
            fowa = fowa
          ),
          index = index,
          sedim = list(
            texture = texture,
            descript = descript
          )
        )
      }

      return(result)
    }
  }
