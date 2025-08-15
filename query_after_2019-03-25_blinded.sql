SELECT
  TO_HEX(SHA256(CAST(a.uid AS STRING))) uid,
  a.date daily_date,
  a.total_daily_gross_revenue,
  a.total_play_time,
  a.ranks_gained,
  a.viplvl,
  a.gem_spend,
  a.dmo,
  a.dos,
  a.co,
  a.ujd,
  a.mission_battlestart,
  a.guild_message,
  a.friend_thank,
  a.friend_request_sent,
  a.friend_invite,
  a.error_client,
  a.error_network,
  a.ad_videofinished,
  b.date survey_date,
  b.survey_name,
  b.rating_response
FROM
(SELECT
  *
FROM
  `XXX-analytics.com_XXX.daily`
WHERE
  uid IN (SELECT DISTINCT uid FROM `XXX-analytics.com_XXX.daily` WHERE date >= DATE('2019-03-04') AND date <= DATE('2019-03-06'))
  AND date >= DATE('2019-03-25')) a
LEFT JOIN
(SELECT
  *
FROM
  `XXX-analytics.DAILY_INGAME_SURVEY_RESPONSES.daily_ingame_survey_responses_*`
WHERE
  _TABLE_SUFFIX >= REGEXP_REPLACE( CAST( PARSE_DATE('%Y-%m-%d',
               CAST(DATE_ADD(CURRENT_DATE(), INTERVAL -120 DAY) AS STRING)) AS STRING), r'-', '')
  AND _TABLE_SUFFIX <= REGEXP_REPLACE( CAST( PARSE_DATE('%Y-%m-%d',
               CAST(DATE_ADD(CURRENT_DATE(), INTERVAL 0 DAY) AS STRING)) AS STRING), r'-', '')
  AND survey_name like '%NPS%'
  AND date >= DATE('2019-03-25')) b
ON
  a.uid=b.uid
  AND a.date=b.date