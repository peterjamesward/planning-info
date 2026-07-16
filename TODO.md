
## BUGS

## TODO

Have an Activity tab which will fit well inside iframe, but links to Lamdera in a new browser window.
> Need to create summary display of activity. 
> Does this require retaining history?
> No, just count received, pending, approved, rejected during last 30 days.

Make it clear what the date represents (which is?).

Look at application types and possibly preemptively filter,
or just allow user to filter by type or status?
> Currently, Types:
Adjacent Authority Consultation
Advertisement Consent
Approval of Details Reserved by Condition
Certificate of Lawful Development Existing
Certificate of Lawful Development Proposed
Full Planning Application
Householder
Listed Building Consent
Non-Material Amendment
Prior Approval (Miscellaneous Acts)
Prior Notification - Larger Home Extension
Removal/Variation of Condition(s)
Trees in a Conservation Area
Works to Protected Trees
advertisement
change_of_use
full_planning
householder
outline_planning
prior_approval

> Statuses:
decided
pending_consideration
received

> Decisions:
approved
not_required
refused
withdrawn

Need to test purging!

Apply colour and text formatting to decision. (More like a rubber stamp?)
> Need to know which values there are and choose colours for decision.
> Show decision if available, with state as default.

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

