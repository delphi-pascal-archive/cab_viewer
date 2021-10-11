unit CABAPI;

interface

uses Classes;

function  CABIsFile (const CABFileName: String): Boolean;
function  CABIsMultiPart (const CABFileName: String): Boolean;
function  CABGetFileCount (const CABFileName: String): Integer;
procedure CABGetFileList (const CABFileName: String; List: TStringList);
procedure CABExtractFile (const CABFileName, DestPath, FileName: String);
procedure CABExtractMultipleFiles (const CABFileName, DestPath: String; List: TStringList);

implementation

uses Windows, SysUtils, FileCtrl;

const
    // FDI Errors
    FDINone                     =       0;              // ok
    FDICabinetNotFound          =       1;              // bad filename passed to FDICopy
    FDINotACabinet              =       2;              // File not in CAB format
    FDIUnknownCABVersion        =       3;              // unknown CAB file version
    FDICorruptCAB               =       4;              // Cabinet file is corrupt
    FDIAllocFail                =       5;              // Out of memory
    FDIBadCompressType          =       6;              // Unknown compression type
    FDIMDIFail                  =       7;              // decompression error
    FDITargetFile               =       8;              // error writing to dest file
    FDIReserveMismatch          =       9;              // reserve size mismatch
    FDIWrongCabinet             =       10;             // incorrect CAB returned
    FDIUserAbort                =       11;             // user aborted

    // FDI notify codes
    ncCABInfo	                =       0;		// General information about cabinet
    ncPartialFile	        =       1;		// First file in cabinet is continuation
    ncCopyFile	                =       2;		// File to be copied
    ncCloseFileInfo	        =       3;      	// close the file, set relevant info
    ncNextCabinet	        =       4;	        // File continued to next cabinet
    ncEnumerate	                =       5;		// Enumeration status

type
    TERF = record
        ErrCode, ErrNo: Integer;
        ErrorPresent: Bool;
    end;

    TFDICabinetInfo = record
        cbCabinet: Integer;                             // size of the archive
        cFolders: Word;                                 // number of folders
        cFiles: Word;                                   // number of files
        setID: Word;                                    // application-defined magic #
        iCabinet: Word;                                 // number of cabinet in set
        fReserve: Integer;                              // has reserved area?
        hasprev: Integer;                               // chained to previous?
        hasnext: Integer;                               // chained to next?
    end;

    TFDINotification = record
        FileSize: Integer;				// uncomp size of the file (ncCopyFile only)
        FileName: PChar;				// name of a file in the CAB
        psz2: PChar;
        psz3: PChar;
        AppValue: Pointer;			        // application supplied value
        fd: Integer;				        // file handle
        Date: Word;				        // file's 16-bit FAT date
        Time: Word;				        // file's 16-bit FAT time
        Attribs: Word;				        // file's 16-bit FAT attributes
        setID: Word;				        // application-defined magic #
        iCabinet: Word;				        // number of this CAB
        iFolder: Word;				        // number of current 'folder'
        FDIError: Integer;				// error code, if any
    end;


var
    erf: TERF;
    CABLib: HModule;
    Info: TFDICabinetInfo;
    DestinationPath: String;
    FDICreate: function (pAlloc, pFree, pOpen, pRead, pWrite, pClose, pSeek: Pointer; cpuType: Integer; var erf: TERF): THandle; cdecl;
    FDIDestroy: function (h: THandle): Bool; cdecl;
    FDIIsCabinet: function (h: THandle; fd: Integer; var info: TFDICabinetInfo): Bool; cdecl;
    FDICopy: function (h: THandle; CabName, CabPath: PChar; Flags: Integer; pNotify, pEncrypt, pUser: Pointer): Bool; cdecl;

// These are the callback routines used by FDI/FCI interface

function MyAlloc (Bytes: Integer): Pointer; cdecl;
begin
    Result := AllocMem (Bytes);
end;

function MyFree (P: Pointer): Pointer; cdecl;
begin
    FreeMem (P);
    Result := Nil;
end;

function MyOpen (FileName: PChar; Mode: Integer): Integer; cdecl;
begin
    Result := _lopen (FileName, Mode);
end;

function MyClose (fd: Integer): Integer; cdecl;
begin
    Result := _lclose (fd);
end;

function MyRead (fd: Integer; buff: Pointer; bytes: Integer): Integer; cdecl;
begin
    Result := _lread (fd, buff, bytes);
end;

function MyWrite (fd: Integer; buff: Pointer; bytes: Integer): Integer; cdecl;
begin
    Result := _lwrite (fd, buff, bytes);
end;

function MySeek (fd: Integer; pos, mode: Integer): Integer; cdecl;
begin
    Result := _llseek (fd, pos, mode);
end;

// These routines simplify the otherwise baroque API

function NewFDIContext: THandle;
begin
    Result := FDICreate (@MyAlloc, @MyFree, @MyOpen, @MyRead, @MyWrite, @MyClose, @MySeek, 0, erf);
end;

function CABIsFile (const CABFileName: String): Boolean;
var
    fd: Integer;
    Context: THandle;
begin
    Result := False;
    Context := NewFDIContext;
    if Context <> 0 then try
        fd := MyOpen (PChar (CABFileName), of_Read);
        if fd <> -1 then try
            Result := FDIIsCabinet (Context, fd, info);
        finally
            MyClose (fd);
        end;
    finally
        FDIDestroy (Context);
    end;
end;

function CABIsMultiPart (const CABFileName: String): Boolean;
begin
    Result := False;
    if CABIsFile (CABFileName) then Result := (Info.iCabinet > 0) or (Info.hasPrev <> 0) or (Info.hasNext <> 0);
end;

function CABGetFileCount (const CABFileName: String): Integer;
begin
    Result := 0;
    if CABIsFile (CABFileName) then Result := Info.cFiles;
end;

function GetFileListCallback (NotifyType: Integer; var Info: TFDINotification): Integer; cdecl;
var
    S: String;
    List: TStringList;
begin
    Result := 0;
    List := Info.AppValue;
    if NotifyType = ncCopyFile then begin
        S := Info.FileName;
        S := S + '|' + IntToStr (Info.FileSize);
        S := S + '|' + IntToStr (MakeLong (Info.Time, Info.Date));
        List.Add (S);
    end;
end;

procedure CABGetFileList (const CABFileName: String; List: TStringList);
var
    Context: THandle;
begin
    List.Clear;
    if CABIsFile (CABFileName) then begin
        Context := NewFDIContext;
        if Context <> 0 then try
            FDICopy (Context, PChar (ExtractFileName (CABFileName)),
                              PChar (ExtractFilePath (CABFileName)),
                              0, @GetFileListCallback, Nil, List);
        finally
            FDIDestroy (Context);
        end;
    end;
end;

function FileExtractSingleCallback (NotifyType: Integer; var Info: TFDINotification): Integer; cdecl;
var
    Path: String;
    TargetFile: PChar;
    FileTime: TFileTime;
begin
    Result := 0;
    TargetFile := Info.AppValue;
    case NotifyType of
        ncCopyFile:             // Is this the file we want to extract?
            if StrIComp (TargetFile, Info.FileName) = 0 then begin
	        Path := DestinationPath + ExtractFileDir (Info.FileName);
        	if not DirectoryExists (Path) then CreateDir (Path);
	        Result := _lcreat (PChar (DestinationPath + StrPas (Info.FileName)), 0);
        	Exit;
            end;

        ncCloseFileInfo:        // Is this the file we want to close?
            begin
                Result := 1;
                // This check is probably redundant, cos we only decompressed
                // a single file but let's play safe....
                if StrIComp (TargetFile, Info.FileName) = 0 then begin
                    DosDateTimeToFileTime (Info.Date, Info.Time, FileTime);
                    SetFileTime (Info.fd, Nil, Nil, @FileTime);
                    MyClose (Info.fd);
                end;
            end;
    end;
end;

procedure CABExtractFile (const CABFileName, DestPath, FileName: String);
var
    Context: THandle;
begin
    if CABIsFile (CABFileName) then begin
        Context := NewFDIContext;
        if Context <> 0 then try
            DestinationPath := DestPath;
            if DestinationPath [Length (DestinationPath)] <> '\' then DestinationPath := DestinationPath + '\';
            FDICopy (Context, PChar (ExtractFileName (CABFileName)),
                              PChar (ExtractFilePath (CABFileName)),
                              0, @FileExtractSingleCallback, Nil, PChar (FileName));
        finally
            FDIDestroy (Context);
        end;
    end;
end;

function FileExtractMultipleCallback (NotifyType: Integer; var Info: TFDINotification): Integer; cdecl;
var
    Path: String;
    List: TStringList;
    FileTime: TFileTime;
begin
    Result := 0;
    List := Info.AppValue;
    case NotifyType of
        ncCopyFile:             // Do we want to extract this file?
            if List.IndexOf (Info.FileName) <> -1 then begin
	        Path := DestinationPath + ExtractFileDir (Info.FileName);
        	if not DirectoryExists (Path) then CreateDir (Path);
	        Result := _lcreat (PChar (DestinationPath + StrPas (Info.FileName)), 0);
        	Exit;
            end;

        ncCloseFileInfo:        // Do we want to close this file?
            begin
                Result := 1;
                if List.IndexOf (Info.FileName) <> -1 then begin
                    DosDateTimeToFileTime (Info.Date, Info.Time, FileTime);
                    SetFileTime (Info.fd, Nil, Nil, @FileTime);
                    MyClose (Info.fd);
                end;
            end;
    end;
end;

procedure CABExtractMultipleFiles (const CABFileName, DestPath: String; List: TStringList);
var
    Context: THandle;
begin
    if CABIsFile (CABFileName) then begin
        Context := NewFDIContext;
        if Context <> 0 then try
            DestinationPath := DestPath;
            if DestinationPath [Length (DestinationPath)] <> '\' then DestinationPath := DestinationPath + '\';
            FDICopy (Context, PChar (ExtractFileName (CABFileName)),
                              PChar (ExtractFilePath (CABFileName)),
                              0, @FileExtractMultipleCallback, Nil, List);
        finally
            FDIDestroy (Context);
        end;
    end;
end;

// Load CABINET.DLL and get pointers to the various entry points...

procedure CABLoad;
begin
    CABLib := LoadLibrary ('cabinet.dll');
    if CABLib = 0 then raise Exception.Create ('Can''t find CABINET.DLL');
    @FDICreate    := GetProcAddress (CABLib, 'FDICreate');
    @FDIDestroy   := GetProcAddress (CABLib, 'FDIDestroy');
    @FDIIsCabinet := GetProcAddress (CABLib, 'FDIIsCabinet');
    @FDICopy      := GetProcAddress (CABLib, 'FDICopy');
end;

procedure CABUnload;
begin
    if CABLib <> 0 then FreeLibrary (CABLib);
end;

initialization
    CABLoad;
finalization
    CABUnload;
end.
