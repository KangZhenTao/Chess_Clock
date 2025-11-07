object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 720
  ClientWidth = 1280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 720
    Align = alLeft
    Caption = 'pnlLeft'
    TabOrder = 0
    OnClick = pnlLeftClick
  end
  object pnlRight: TPanel
    Left = 640
    Top = 0
    Width = 640
    Height = 720
    Align = alRight
    Caption = 'pnlRight'
    TabOrder = 1
    OnClick = pnlRightClick
    ExplicitLeft = 1095
  end
end
