object Form1: TForm1
  Left = 226
  Top = 127
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CAB Viewer'
  ClientHeight = 514
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object FileCount: TLabel
    Left = 10
    Top = 10
    Width = 3
    Height = 16
  end
  object Button1: TButton
    Left = 8
    Top = 480
    Width = 441
    Height = 25
    Caption = 'Open...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object FileList: TListView
    Left = 8
    Top = 8
    Width = 441
    Height = 465
    Color = 8404992
    Columns = <
      item
        Caption = 'FileName'
        Width = 123
      end
      item
        Caption = 'Uncompressed size'
        Width = 160
      end
      item
        Caption = 'Timestamp'
        Width = 123
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ViewStyle = vsReport
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'CAB'
    Filter = 'CAB Files (*.CAB)|*.CAB'
    Left = 16
    Top = 40
  end
end
