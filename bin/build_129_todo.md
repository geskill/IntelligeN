#IntelligeN 2009 TODO list

This todo list contains all useful ideas for improvement, not necessarily for the next release.

###Core

- [ ] Handle DDos protection from Cloudflare

- [ ] Allow multiple downloads with HTTPManager at the same time

- [ ] Allow to change HTTPManager settings

- [ ] Allow to change HTTPManager implementor on-the-fly

- [ ] Optimize HTTPManager log amount (i.e. ALL, SOME)

- [ ] Improve handling of unknown file hosts (those not defined in hoster.xml)

- [ ] Add custom fields in ITabSheetData that hold compiled IScript (required for WordPress custom files powered by IScript)

- [ ] Add direct index based link access (i.e. IMirror[\<Index\>].Directlink[\<Index\>].Link[\<Index\>])

- [ ] Add direct index based filename access (i.e. Mirror[\<Index\>].Directlink[\<Index\>].FileName[\<Index\>])

- [ ] Implement image resizing instead of depending on image hosts resize

###SDK

######Content-Management-Systems

- [ ] Add output link/id of created threads and/or posts

- [ ] Add detection for post prohibition and wait this amount of time

- [ ] Add password protected subforums for vBulletin

######Crawler

- [ ] Update missing plugins to match new interface (incl. full update of Amazon.com/.de)

######Crypter

- [ ] Add flag if a user account is required

- [ ] Add distinction flag if username/password or API-Key is required

######FileFormats

- [ ] intelligen.xml.2 improve file saving (load existing XML file and assign new properties instead of create a complete new XML file)

######FileHoster

- [ ] Add boolean result flag for file check

######ImageHoster



###View

- [ ] Add simpler hoster ranking not defined by website (i.e. FileHoster ranking)

- [ ] Implement a silent error logger

- [ ] Improve WebsiteEditor control arrangement in general tab

- [ ] Add indication in the GUI if image hoster upload was successful or not (i.e. green/red shape at the icon)

- [ ] Add possibility in WebsiteEditor to base general settings upon the TypeID (i.e. PCGames with intelligent_posting and Software not)
