unit CABFile;

interface

uses SysUtils, Classes;

type
    ECABError = class (Exception);

    TCabinet = class (TComponent)
    private
        fCABFileName: String;
        fFileList: TStringList;
        fFileCount: Integer;
        procedure Clear;
        function GetFileName (Index: Integer): String;
        function GetFileSize (Index: Integer): String;
        function GetFileDate (Index: Integer): String;
        procedure SetCABFileName (const Value: String);
    public
        constructor Create (AOwner: TComponent); override;
        destructor Destroy; override;
        property CABFileName: String read fCABFileName write SetCABFileName;
        property FileCount: Integer read fFileCount;
        property FileName [Index: Integer]: string read GetFileName; default;
        property FileSize [Index: Integer]: string read GetFileSize;
        property FileDate [Index: Integer]: string read GetFileDate;
    end;

implementation

uses CABAPI;

// TCabinet

function GetField (S: String; Num: Integer): String;
var
    Idx: Integer;
begin
    while Num > 0 do begin
        Idx := Pos ('|', S);
        if Idx > 0 then Delete (S, 1, Idx);
        Dec (Num);
    end;

    Idx := Pos ('|', S);
    if Idx > 0 then Delete (S, Idx, MaxInt);
    Result := S;
end;

constructor TCabinet.Create (AOwner: TComponent);
begin
    Inherited Create (AOwner);
    fFileList := TStringList.Create;
end;

destructor TCabinet.Destroy;
begin
    Clear;
    fFileList.Free;
    Inherited Destroy;
end;

procedure TCabinet.Clear;
begin
    fCABFileName := '';
    fFileList.Clear;
end;

procedure TCabinet.SetCABFileName (const Value: String);
begin
    Clear;
    if not FileExists (Value) then raise ECABError.Create ('Specified CAB file not found') else
    if not CABIsFile (Value) then raise ECABError.Create ('Not a valid CAB file') else
    if CABIsMultiPart (Value) then raise ECABError.Create ('Multi-part CAB files not supported') else begin
        fCABFileName := Value;
        fFileCount := CABGetFileCount (fCABFileName);
        CABGetFileList (fCABFileName, fFileList);
    end;
end;

function TCabinet.GetFileName (Index: Integer): String;
begin
    if (Index >= 0) and (Index <= fFileCount) then Result := GetField (fFileList [Index], 0);
end;

function TCabinet.GetFileSize (Index: Integer): String;
begin
    if (Index >= 0) and (Index <= fFileCount) then Result := GetField (fFileList [Index], 1);
end;

function TCabinet.GetFileDate (Index: Integer): String;
begin
    if (Index >= 0) and (Index <= fFileCount) then
        Result := FormatDateTime ('', FileDateToDateTime (StrToInt (GetField (fFileList [Index], 2))));
end;

end.
