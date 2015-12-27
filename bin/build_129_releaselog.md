#IntelligeN 2009 BUILD 129 release log

###ALPHA 1

######major enhancements

* Crypter ThreadPool with OTL

* JScriptEditor

* System wide HTTPManager

######minor enhancements

* SOCKS 4A

* Browser like mouse over Tab close button fade in/out

* Smooth Tab Hints

* More XXX Genres


###ALPHA 2

######major enhancements

* Changed Boolean type inside global Interfaces to WordBool for COM* compatibility

######minor enhancements

* Version/Build information inside the plugin (pre-error detection for wrong application // plugin build combination)

* HTTP Proxy BasicAuthentication support

######other changes

* Removed support for wCrypt.in crypting service (service unavailable)


###ALPHA 3

######major enhancements

* Crawler ThreadPool with OTL

######other changes

* Update to latest indy build (i.e. fixes xRel.to UTF-8 encoding) any side effects?


###ALPHA 4

######major enhancements

* Hoster ThreadPool with OTL

######minor enhancements

* HTTPManager didn't set UserAgent, CustomHeaders

* http://forums2.atozed.com/viewtopic.php?f=7&t=26262

* Made Tinypic.com useable without CAPTCHA

######other changes

* On first startup Xrel.to and Releasename Crawler Plugins ll be inserted on top of the crawler list; Releasename Crawler is enabled, too

* Removed support for enterupload.com, filefrog.to, filesonic.com, ifile.it, kickload.com, loadfiles.in, megaupload.com, oron.com, ugotfile.com, x7.to, wupload.com, zshare.net hosting service (service unavailable)

* Removed support for Pic.ms, Youpic.in uploading service (service unavailable)


###ALPHA 5

######major enhancements

* Introducing series processing (starting with Crawler, Cryper actions) [this will be later maybe only available for professional users !]

* HTTP Logger basic features

######minor enhancements

* Added some HTML codes (especially for Amazon.com/.de)

* Fixed critical bug inside Hoster ThreadPool

* Cannot close current tab when crawler is active in this tab

######other changes

* Updated openSSL librarys


###ALPHA 5.5

######minor enhancements

* Added runtime information for Amazon.com/.de

* Added szene names to Releasename Crawler: BRRIP, PPVRIP, SCR, WORKPRINT

######other changes

* Renamed einsadvdch.dll to 1advdch.dll


###ALPHA 6

######major enhancements

* read/write controls value delivered from IBasic should now be thread-safe

######minor enhancements

* Fixed critical bug inside crack detection

* Fixed bug inside HTTPManager Clone/FollowUpClone


###ALPHA 7

######minor enhancements

* Improved ThreadPoolManager blacklist function (added whitelist)

* Fixed bug inside Hoster ThreadPool (wrong SLEEP MSG id)

* Fixed bugs inside CMS plugins when trying to make request upon a non existing


###ALPHA 8

######minor enhancements

* Updated hoster.xml removed deleted hosters, added Cloudnator.com, Extabit.com, Filepost.com, Rapidgator.net, Terabit.to

* Fixed bug inside global CMS login


###ALPHA 9

######minor enhancements

* Crawler ThreadPool updates the control values now inside thread because values are now thread-safe

* Improved new tab creation (with 10 directlinks: ~1 sec faster as ALPHA 8) [still not as fast as BUILD 128.4]

* Added some HTML codes (especially for Amazon.com/.de)


###ALPHA 10

######major enhancements

* Added IMDB.com crawler plugin

######minor enhancements

* Added Extabit.com, Filepost.com, Rapidgator.net, Terabit.to hoster plugins

* Improved new website wizard (handles Refresh-header)

* Updated sceper.eu (before scenereleases.info) crawler plugin

* Fixed bug when creating new tab with MirrorPosition = Top

* Fixed potential HTTPManager memory leak (file-stream elements)


###ALPHA 11

######minor enhancements

* Improved detection when subjectfile not exists while posting via publish/publish all

* Fixed HTTPManager POST param bug using file-streams

* Fixed HTTPManager memory leak (FNewHTTPProcess event)

* Fixed imdb.com (introduced deep_search)


###ALPHA 12

######minor enhancements

* Improved new website wizard (detects needed www. in url)

* Fixed bug calling publish all (with min 2 tabs) after publishing 2 or more uploads at the same time.

* Fixed bug deleting a publish filter

* Fixed imdb.com (cover @@)


###ALPHA 13

######major enhancements

* Introducing MultiImage Hoster system [this will be later maybe only restricted available for personal users !]

* OLE Drop - drop text from Browsers into controls

######minor enhancements

* Improved HTTPManager GetResult (access to response stream)

* Updated adultdvdempire.com (added genres)

* Updated Amazon.com/.de (new silver design)

* Fixed bug while publishing using a filter, where disabled CMS aren't ignored

* Fixed bug inside TXMLSerializer (canceling reading if property is missing)

* Fixed bug inside TThreadPoolManager (deleting finished item from blacklist, if in list)

* Removed hoster.xml id definitions (hoster icons now saved inside hoster dll -> more dynamic)


###ALPHA 14

######major enhancements

* Added Hoster ranked-white-/blacklist (only website editor, needs iscript/cms implementation)

* Added CustomFields for WordPress CMS (only static, need iscript implementation)

######minor enhancements

* Fixed bug inside HTTPManager where fields of 'multipart/form-data' params got a filename

* Improved Discuz! CMS Plugin (icons, addtoblog)

* Improved ipb3 CMS Plugin (ipsTags_prefix=1)

* Fixed wbb3 CMS Plugin (unrecognized successful login)

* Fixed bug where clicking manuell upload to imagehoster fails


###ALPHA 15

######major enhancements

* Added Filter (only website editor, needs publish implementation)

######minor enhancements

* Improved vBulletin CMS Plugin (need_prelogin setting (some websites needs to visit before login))

* Fixed little layout height problem in WebsiteEditor (Hosters)


###BETA 1

######major enhancements

* Introducing frameworkX since compiling/linking all components into a single framework fails. Now framework.bpl holds all basic classes/components. It's planed to become a static file that don't needs to change anymore.
  All changes through new or updated 3rd-party classes/components are catched by frameworkX. All plugins only linked to framework (except app-plugins which use 3rd-party components [i.e. MirrorSort]).  This improves a
  comprehensive plugin-system for the future.

* Introducing three-pillar-structure (Data/Design/Preview) with new IScriptDesigner and the fantastic HTMLPreview

* Introducing DirectoryMonitor

* Publish ThreadPool with OTL (PublishManager with PublishQueue)

* PublishController with "Live-Access-Management" (website based Image/Hoster White-/Blacklist and Control "also-known-as" -> "known-to-write-as" functionality possible in the feature).

######minor enhancements

* Updated hoster.xml added 4fastfile.com, Bayfiles.com, Cloudzer.net, Ddlstorage.com, Easybytez.com, Fiberupload.com, Filecloud.ws, Filegag.com, Filesega.com, Henchfile.com, Hitfile.net, Secureupload.eu, Uload.to, Ultramegabit.info

* Renamed crypter "Account name" label to "Account name / API key" since many crypters use API key nowadays


###BETA 2

######major enhancements

* Introducing IScript code formatting based upon Artistic Style

* Improved MultiImage Hoster system (status image hoster icon inside image control)

######minor enhancements

* Added single-publish-item preview/publish

* Added CAPTCHA bridge for WebsiteEditor

* Fixed MyBB logged-in, need-CAPTCHA detection

* Updated Amazon.com/.de (description)


###BETA 2.5

######minor enhancements

* Fixed IScript code formatting based upon Artistic Style

* Fixed ICMS/IWebsite IScript output (mixed up params)


###BETA 2.6

######minor enhancements

* Improved access to all hosters inside WebsiteEditor blacklist (loading from hoster.xml)

* Improved detection for missing subject/message files for publish progress

* Improved xRel.to description crawling (relative links)

* Fixed publish of single item, while there disabled items

* Fixed process of adding new website files

* Fixed "Argument out of Range" error for HTW, LoadIT, N3m0CMS and uCMS plugins (max. X mirror allowed)


###BETA 3

######major enhancements

* Huge interface changes (i.e. changed compiler directive to safecall)

* Introducing MultiEvents sub-project

* Introducing local image upload (incl. updated all image hoster plugins)

* Improved HTTPManager using multi event handling

######minor enhancements

* Added Cloudzer.net hoster plugin

* Improved HTTPManager using DoRequest method for maintainability (instead of override several GET and POST methods)

* Fixed bug inside HTTPManager (THTTPParams.Clone worked not correctly)


###BETA 4

######major enhancements

* Added (Auto)-Update

######minor enhancements

* Improved IXML2 export for partsize, parts, statusimage* and statusimagetext* for directlinks/crypters*

* Improved IScript runtime error handling

* Improved SetFocus (check can focus before)

* Improved Loading CMS Website settings

* Improved check/uncheck all from website settings

* Fixed a bug where the publish list got empty if a not active tab was removed before.

* Fixed a bug where changes in the mirros do not update the file changed status

* Fixed bug of changing the subject or message file of a website while using this website in the CODE/DESIGN view. (Hereby the subject or message did not recieve the update)

* Fixed possible bug inside HTTPManager (THTTPOptions.Create could be nil)


###BETA 5

######major enhancements

* Tremendous interface changes

* Added new base categories EBook, PlayStation4, PlayStationVita, WiiU and XboxOne

* Added new controls ICreator (before IArtist), IDirector and IPublisher

* Improved Plugin Error handling

* Removed DirectoryMonitor (need re-work, planned for next BUILD)

######minor enhancements

* Added new status images incl. new image for temporary offline

* Added tiff image file support

* Added fastpic.ru image hoster

* Added possibility to visit uploaded image on picture mirror via double click on the image hoster icon

* Improved logo

* Improved TypeID icons

* Improved templates_type files

* Improved FirstStart performance

* Improved crawler interface by adding function that ensures the requirement of certain controls

* Improved wiiboxart.com crawler

* Updated to TLS 1.2 SSL in HTTPManager

* Fixed a bug in the HTTP logger not showing the path of file parameters

* Fixed a bug of adding a new tab while having MirrorPosition = mpTop

* Fixed a bug switching the view from code to data, while a hint is displayed in code view

* Fixed IScript Editor website OnChange

* Fixed IScirpt editor enabling/disabling a CMS

* Fixed enabled status of IScript editor buttons at launch

* Fixed wbb4 cms plugin to support version 4.1.8

* Fixed linkcrypt.ws crypter plugin

* Fixed share-online.biz hoster plugin

* Removed base categories GameCube, PlayStation2, PlayStationPortable and Xbox


###BETA 6

######major enhancements

* Added Error Logger

* Added signature creation

* Added possibility for multiple simultaneous connections with HTTPManager

######minor enhancements

* Added indication in the GUI if the file hoster check, crypter crypt/check was not successful

* Added indication in the GUI if the image hoster upload was successful or not

* Added possibility to change website in code or design view via double click on website in publish window

* Added host and subject in CAPTCHA and intelligent_posting-Helper window

* Added URLEncode/URLDecode to IScript

* Added NFOStripper

* Improved performance

* Improved TThreadManager thread-safety

* Improved IScript Website selection box using host including the path to distinguish among websites on the same host

* Improved ExtractUrlHost using TIdURI. (therefore Indy package is now part of framework.bpl)

* Improved handling of non-normalized host

* Improved OleInitialize/CoInitializeEx usage

* Improved control filters in WebsiteEditor

* Improved app interface to check, whether a app plug-in can be unloaded

* Improved ipb2 cms plug-in

* Improved phpbb3 login error message

* Improved phpbb3 forum id recognition if first forum is already sub-forum or portal mod is used

* Improved releasename crawler (thx to Sena_x3)

* Improved xRel.to error detection

* Fixed a bug in the update process, doing the update at the next start

* Fixed a bug in the website wizard

* Fixed a bug drawing a new tab with crypters

* Fixed threaded draw process for images in Control Editor

* Fixed issue not displaying any IPicture value because of using a some empty value instead of available image link

* Fixed issue not using imagehoster mirror if filters-tag in website xml file is undefined or empty

* Fixed filter selection bug in WebsiteEditor

* Fixed intelligent_posting (intelligent_posting_keepshortwords is only allowed in search title generation, not as match title for comparison with search list)

* Fixed bug not aborting using intelligent_posting_helper after first redo-search

* Fixed wbb4, XenForo intelligent_posting

* Fixed XenForo login

* Fixed uRelink.us folder images

* Fixed loading intelligen.xml.2 files with no templatetype

* Changed IScriptEditor tab size to 2 (before 4)

* Updated HTTPManager (among others fixed RAM issue)

* Updated about dialog

* Updated openSSL librarys


###BETA 7

######major enhancements

* Improved IScriptEditor performance

######minor enhancements

* Added AddTab and RemoveTab events in the PageController

* Added OnCrawlingFinished event

* Added "short_title" subjects

* Added redirect in XenForo settings

* Improve PublishQueue (delete old entries)

* Improved Main and Settings form layout

* Improved IScript generation (NeedFinalQuote)

* Improved PublishJob caption

* Implemented SettingsHelper with old RTTI

* Improved WebsiteEditor using DevExpress LayoutControl

* Improved loading app plug-ins

* Fixed issue not clearing the publish view after closing last tab

* Fixed IScriptEditor issue with active edit reference

* Fixed PublishRetry

* Fixed issue canceling publish jobs

* Fixed logger multithreading issue

* Fixed XenForo DoAnalyzePost

* Updated HTTPManager (fixed UpdateCapacity function)

* Updated hoster.xml removed deleted hosters


###RTM

######major enhancements



######minor enhancements

* Improved default crawled values