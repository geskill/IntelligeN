#IntelligeN 2009 TODO list

This todo list contains all useful ideas for improvement, not necessarily for the next release.

Checked items are planned for next release.

###Core

- [ ] Allow to change HTTPManager implementor for a single request (settings record)

- [ ] Implement BackupManager to store edited and published articles with website thread ids for later article editing

- [ ] Implement non-visual "default" control that holds title of current tab (i.e. delegation for releasename or whatever)

- [ ] Add direct index based link access (i.e. IMirror[\<Index\>].Directlink[\<Index\>].Link[\<Index\>])

- [ ] Add direct index based filename access (i.e. Mirror[\<Index\>].Directlink[\<Index\>].FileName[\<Index\>])

- [ ] Add a website project file format to store website XML, subject+message TXT and optional credentials

- [ ] Implement access for APP-plugins to add function to the IScript

- [ ] Implement local image resizing instead of depending on image hosts resize

- [ ] Implement a default path for saved XML files

- [ ] Implement a priority index for websites to publish articles first on specific websites

- [ ] Improve saving and loading files using default path

- [ ] Improve update system to only close required IntelligeN.exe instance

###SDK

###### APP



######Content-Management-Systems

- [ ] Add interface in order to edit articles

- [ ] Add output link/id of created threads and/or posts

- [ ] Add detection for post prohibition and wait this amount of time

- [ ] Add password protected subforums for vBulletin

######Crawler



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

- [ ] Add progress bar or indication for crawling overall progress

- [ ] Add possibility in WebsiteEditor to base general settings upon the TypeID (i.e. PCGames with intelligent_posting and Software not)
