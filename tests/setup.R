# first response heroes
# question design thru publish
# time 'doxed by zeno
library(haven)

sas_url <- "https://www.cdc.gov/nchs/data/rss/rss1_puf_t1.sas7bdat"
	
rss_tbl <- read_sas( sas_url )

rss_df <- data.frame( rss_tbl )

names( rss_df ) <- tolower( names( rss_df ) )

rss_df[ , 'one' ] <- 1
# rss_fn <- file.path( path.expand( "~" ) , "RSS" , "this_file.rds" )
# saveRDS( rss_df , file = rss_fn , compress = FALSE )
# rss_df <- readRDS( rss_fn )
library(survey)

options( survey.lonely.psu = "adjust" )

rss_design <- 
	svydesign( 
		~ p_psu , 
		strata = ~ p_strata , 
		data = rss_df , 
		weights = ~ weight_m1 , 
		nest = TRUE 
	)
rss_design <- 
	
	update( 
		
		rss_design , 
		
		how_often_use_cleaner_purifier =
			factor(
				ven_use ,
				levels = c( -9:-6 , 0:3 ) ,
				labels = 
					c( "Don't Know" , "Question not asked" , "Explicit refusal/REF" , 
					"Skipped/Implied refusal" , "Never" , "Rarely" , "Sometimes" , "Always" )
			) ,
		
		has_health_insurance = ifelse( p_insur >= 0 , p_insur , NA ) ,
		
		metropolitan = 
			factor( as.numeric( p_metro_r == 1 ) , levels = 0:1 , labels = c( 'No' , 'Yes' ) )
		
	)
sum( weights( rss_design , "sampling" ) != 0 )

svyby( ~ one , ~ metropolitan , rss_design , unwtd.count )
svytotal( ~ one , rss_design )

svyby( ~ one , ~ metropolitan , rss_design , svytotal )
svymean( ~ p_hhsize_r , rss_design )

svyby( ~ p_hhsize_r , ~ metropolitan , rss_design , svymean )
svymean( ~ how_often_use_cleaner_purifier , rss_design )

svyby( ~ how_often_use_cleaner_purifier , ~ metropolitan , rss_design , svymean )
svytotal( ~ p_hhsize_r , rss_design )

svyby( ~ p_hhsize_r , ~ metropolitan , rss_design , svytotal )
svytotal( ~ how_often_use_cleaner_purifier , rss_design )

svyby( ~ how_often_use_cleaner_purifier , ~ metropolitan , rss_design , svytotal )
svyquantile( ~ p_hhsize_r , rss_design , 0.5 )

svyby( 
	~ p_hhsize_r , 
	~ metropolitan , 
	rss_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ p_agec_r , 
	denominator = ~ p_hhsize_r , 
	rss_design 
)
sub_rss_design <- subset( rss_design , sun_useface >= 3 )
svymean( ~ p_hhsize_r , sub_rss_design )
this_result <- svymean( ~ p_hhsize_r , rss_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ p_hhsize_r , 
		~ metropolitan , 
		rss_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( rss_design )
svyvar( ~ p_hhsize_r , rss_design )
# SRS without replacement
svymean( ~ p_hhsize_r , rss_design , deff = TRUE )

# SRS with replacement
svymean( ~ p_hhsize_r , rss_design , deff = "replace" )
svyciprop( ~ has_health_insurance , rss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( p_hhsize_r ~ has_health_insurance , rss_design )
svychisq( 
	~ has_health_insurance + how_often_use_cleaner_purifier , 
	rss_design 
)
glm_result <- 
	svyglm( 
		p_hhsize_r ~ has_health_insurance + how_often_use_cleaner_purifier , 
		rss_design 
	)

summary( glm_result )
result <-
	svymean(
		~ as.numeric( ven_use > 0 ) ,
		subset( rss_design , ven_use >= 0 )
	)

stopifnot( round( coef( result ) , 3 ) == .379 )

stopifnot( round( confint( result )[1] , 3 ) == 0.366 )

stopifnot( round( confint( result )[2] , 3 ) == 0.393 )
library(srvyr)
rss_srvyr_design <- as_survey( rss_design )
rss_srvyr_design %>%
	summarize( mean = survey_mean( p_hhsize_r ) )

rss_srvyr_design %>%
	group_by( metropolitan ) %>%
	summarize( mean = survey_mean( p_hhsize_r ) )
