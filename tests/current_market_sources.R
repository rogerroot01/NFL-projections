e <- new.env(parent=globalenv());source('app.R',local=e)
toy <- data.frame(game_id=c('2026_6_CAR_PHI','2026_6_SEA_DEN'),spread_line=c(1.5,-2.5),
 total_line=c(41.5,40.5),home_score=c(NA,17),away_score=c(NA,14))
updated <- e$apply_current_lines(toy,preserve_completed=TRUE)
early <- e$apply_backtest_lines(toy,'early')
stopifnot(updated$spread_line[1]==-3.5,updated$total_line[1]==39.5,
 updated$spread_line[2]==-2.5,updated$total_line[2]==40.5,
 early$spread_line[1]==1.5,early$total_line[1]==41.5,
 all(updated$home_score==toy$home_score,na.rm=TRUE),all(updated$away_score==toy$away_score,na.rm=TRUE))
cat('WRANGLER_CURRENT_AND_POSTING_SOURCES_PRESERVED\n')
