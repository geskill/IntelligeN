{ ********************************************************
  *                                     IntelligeN CORE  *
  *  Global base constants                               *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uBaseConst;

interface

const
  Programm = 'IntelligeN';
  ProgrammName = Programm + ' 2009';

type
  TContentStatus = (
    {$REGION 'Documentation'}
    /// <summary>
    ///   (default) Not checked by IntelligeN
    /// </summary>
    {$ENDREGION}
    csNotChecked,
    {$REGION 'Documentation'}
    /// <summary>
    ///   Not checked by the service or unknown status
    /// </summary>
    {$ENDREGION}
    csUnknown,
    {$REGION 'Documentation'}
    /// <summary>
    ///   The service reported all files are offline
    /// </summary>
    {$ENDREGION}
    csOffline,
    {$REGION 'Documentation'}
    /// <summary>
    ///   The service reported all files are online
    /// </summary>
    {$ENDREGION}
    csOnline,
    {$REGION 'Documentation'}
    /// <summary>
    ///   The service reported all files are online, but currently offline due
    ///   to maintenance
    /// </summary>
    {$ENDREGION}
    csTemporaryOffline,
    {$REGION 'Documentation'}
    /// <summary>
    ///   The service reported some files are online and some are offline
    /// </summary>
    {$ENDREGION}
    csMixedOnOffline);
{$REGION 'Documentation'}
  /// <summary>
  /// Control type definition of all controls in IntelligeN. This is required
  /// to identify the exact control type along the IntelligeN eco-system.
  /// </summary>
{$ENDREGION}
  TControlID = (cReleaseName, cReleaseDate, cTags, cTitle, cCreator, cDirector, cPublisher, cPicture, cTrailer, cSample, cNotes, cPassword, cAudioBitrate, cAudioBitrateType, cAudioEncoder, cAudioSamplingRate, cAudioStream, cGenre, cLanguage, cRuntime,
    cVideoCodec, cVideoStream, cVideoSystem, cNFO, cDescription);

  TControlIDs = set of TControlID;
{$REGION 'Documentation'}
  /// <summary>
  /// File system definition of the IntelligeN folders and sub-folders.
  /// </summary>
{$ENDREGION}
  TFileSystem = (fsNull, fsRoot, fsConfig, fsPlugins, fsSettings, fsCMS, fsCMSSubject, fsCMSMessage, fsSite, fsType);
{$REGION 'Documentation'}
  /// <summary>
  /// Tab type definition of the different views.
  /// </summary>
{$ENDREGION}
  TTabViewType = (vtNull, vtData, vtCode, vtPreview);
{$REGION 'Documentation'}
  /// <summary>
  /// Type definition of the different release categories.
  /// </summary>
{$ENDREGION}
  TTypeID = (cAudio, cEBook, cGameCube, cMovie, cNintendoDS, cPCGames, cPlayStation3, cPlayStation4, cPlayStationPortable, cSoftware, cWii, cWiiU, cXbox360, cXboxOne, cXXX, cOther);

  TTypeIDs = set of TTypeID;

const
  cConsole: set of TTypeID = [cGameCube, cNintendoDS, cPlayStation3, cPlayStation4, cPlayStationPortable, cWii, cWiiU, cXbox360, cXboxOne];
  cGames: set of TTypeID = [cGameCube, cNintendoDS, cPCGames, cPlayStation3, cPlayStation4, cPlayStationPortable, cWii, cWiiU, cXbox360, cXboxOne];

implementation

end.
