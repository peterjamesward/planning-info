
## BUGS

## TODO

Change logic on constraint filter so any matches will work.

Have an Activity tab which will fit well inside iframe, but links to Lamdera in a new browser window.
> Need to create summary display of activity. 
> Does this require retaining history?
> No, just count received, pending, approved, rejected during last 30 days.

Need to test purging!

## Backlog

Post an email when a new HA7 submission arrives, maybe any state change,
as determined by a new history entry not just a timestamp.
https://github.com/MartinSStewart/elm-email
(Postmark is free for 100/month.)

Handle a 429 error code for quota ...
> If you exceed 10 requests per minute or your monthly quota, the API returns 429 Too Many Requests with a Retry-After header indicating when to try again.
> See https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse

Hide Google Maps API key.

## Optimisations

