SELECT
  TO_HEX(SHA256(CAST(a.uid AS STRING))) uid, # hashed unique user identifier
  a.date daily_date, # calendar date
  a.total_daily_gross_revenue, # gross revenue generated / $ spent by the user on the day
  a.total_play_time, # total time spent playing by the user on the day
  a.ranks_gained, # how many ranks (in the game's player ranking system) did the user gain that day
  a.viplvl, # what highest vip level was the user that day, users that spend more have a higher vip level, similar to a loyalty program
  a.gem_spend, # how many gems (the game's main premium currency sold in in-app purchases) did the user spend on loot boxes that day
  a.dmo, # user's device model used for playing
  a.dos, # user's device operating system
  a.co, # country the user was playing from
  a.ujd, # when did the user first start playing the game
  a.mission_battlestart, # the game evolves around fighting "bosses" in missions, either socially or solo; this entry counts how many such missions the user started that day
  a.guild_message, # how many messages did the user send to her guild; guilds are the game's social entity / teams
  a.friend_thank, # in addition to being in a guild, users can have in-game friends; these friends will often overlap with the (up to 30) members of the user's guild, but they don't have to; this column counts how many thanks the user sent to their friends that day
  a.friend_request_sent, # how many friend requests did the user send that day
  a.friend_invite, # how many friend invites did the user receive that day
  a.error_client, # how many client errors did the user experience that day; a client error does not have to be noticeable to the user but generally: the more errors the worse the user experience
  a.error_network, # count of connectivity problems experienced by the user
  a.ad_videofinished, # how many video ads did the user watch that day
  b.date survey_date, # date of survey response, missing if no survey active or user did not respond
  b.survey_name, # type of survey run/responded to
  b.rating_response # user's response; subtract 1 to get the values shown to the user in the app
FROM
(SELECT
  *
FROM
  `XXX-analytics.com_XXX.daily`
WHERE
  uid IN (SELECT DISTINCT uid FROM `XXX-analytics.com_XXX.daily` WHERE date >= DATE('2019-03-04') AND date <= DATE('2019-03-06')) ) a
LEFT JOIN
(SELECT
  *
FROM
  `XXX-analytics.DAILY_INGAME_SURVEY_RESPONSES.daily_ingame_survey_responses_*`
WHERE
  _TABLE_SUFFIX >= REGEXP_REPLACE( CAST( PARSE_DATE('%Y-%m-%d',
               CAST(DATE_ADD(CURRENT_DATE(), INTERVAL -60 DAY) AS STRING)) AS STRING), r'-', '')
  AND _TABLE_SUFFIX <= REGEXP_REPLACE( CAST( PARSE_DATE('%Y-%m-%d',
               CAST(DATE_ADD(CURRENT_DATE(), INTERVAL 0 DAY) AS STRING)) AS STRING), r'-', '')
  AND survey_name like '%NPS%') b
ON
  a.uid=b.uid
  AND a.date=b.date