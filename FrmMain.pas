unit FrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    FileCount: TLabel;
    FileList: TListView;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses CABFile;

var
    cab: TCabinet;

procedure TForm1.FormCreate(Sender: TObject);
begin
    cab := TCabinet.Create (Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    cab.Free;
end;

procedure TForm1.Button1Click (Sender: TObject);
var
    Idx: Integer;
    Item: TListItem;
begin
    if OpenDialog.Execute then begin
        FileList.Items.Clear;
        cab.CABFileName := OpenDialog.FileName;
        FileCount.Caption := 'Files in CAB = ' + IntToStr (cab.FileCount);
        FileList.Items.BeginUpdate;
        try
            for Idx := 0 to cab.FileCount - 1 do begin
                Item := FileList.Items.Add;
                Item.Caption := cab [Idx];
                Item.SubItems.Add (cab.FileSize [Idx]);
                Item.SubItems.Add (cab.FileDate [Idx]);
            end;
        finally
            FileList.Items.EndUpdate;
        end;
    end;
end;

end.
