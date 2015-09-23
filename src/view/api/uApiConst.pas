unit uApiConst;

interface

uses
  // Delphi
  Classes,
  // Common
  uConst;

const
  http: string = 'http://';

  Homepage = 'http://intelligen2009.com/';

  /// when changing these names, crack detection needs changing
  ProgrammVersion: array [0 .. 3] of string = ('Personal', 'Professional', 'Enterprise', 'Architect');

  BackupFilename = 'releases.sdb';

type
  TComponentParserMode = (cpNone, cpLight, cpFull);

resourcestring
  StrClose = 'Close';
  StrDocument = 'document';
  StrSettings = 'Settings';
  StrAdd = 'Add';
  StrRemove = 'Remove';
  StrAll = 'All';
  StrSort = 'Sort';
  StrRemoveDouble = 'Remove double';
  StrNotifyMissing = 'Notify missing';
  StrNewDatabaseConnect = 'New database connection';
  StrOverrideSPlugin = 'Override %s plugin?';
  StrThisPluginIsIncom = 'This plugin is incompatible for this version %s ! (Plugin build: %s)';
  StrThisPluginBelongs = 'This plugin belongs to another plugin interface';
  StrUnknownSPlugin = 'Unknown %s plugin';
  StrPluginDamaged = 'Plugin damaged! (%s)';
  StrNewItem = 'New item';
  StrEditItem = 'Edit item';
  StrNewItemValue = 'New item value:';
  StrRemoveItem = 'Remove item';
  StrRemoveSelectedItem = 'Remove selected item?';
  StrBeFairAndUpgrade = 'Be fair and upgrade to the professional edition. Thanks.';
  StrDefault = '<default>';
  StrDirectlinks = 'Directlinks';
  StrTextFiles = 'Text files';
  StrImageFiles = 'Image files';
  StrContainerFiles = 'Container files';
  StrAllFiles = 'All files';

implementation

end.
