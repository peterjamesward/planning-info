# Simple planning application viewer

This was developed specifically to provide some visibility into new planning application for the members of the Stanmore Society. 

Harrow Council does not offer an API but I found plannexus (amongst others similar) which aggregates planning data across all UK councils.

It's fairly easy to extract planning applications by (for example) postcode sector and date. It also reveals key information such as whether the site is in an AONB, SSSI, or Green Belt. This is particularly pertinent to our members, who may wish to express their views to the Council.

To keep within the free-tier, we:
- only use the HA7 postcode
- only pull applications submitted in the last 28 days
- only pull once each working day
- cache the data and share across multiple clients
- wait seven seconds between API calls.

To do the central caching in Elm, I'm using Lamdera, which is awesome. There is also a free tier there which limits the uptime to 16 hours a day and the total data to 1MB.

These restrictions may be relaxed if the Society decides to adopt in and invest in running this service.
