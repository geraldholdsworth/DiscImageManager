object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 114
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object CreditsPanel: TPanel
    Left = 0
    Top = 0
    Width = 433
    Height = 114
    Align = alClient
    TabOrder = 0
    object Label7: TLabel
      Left = 1
      Top = 1
      Width = 431
      Height = 40
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Disc Image Reader'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 453
    end
    object Label8: TLabel
      Left = 1
      Top = 41
      Width = 431
      Height = 13
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'written by Gerald J Holdsworth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 128
      ExplicitTop = 72
      ExplicitWidth = 31
    end
    object Label9: TLabel
      Left = 1
      Top = 54
      Width = 431
      Height = 19
      Align = alTop
      Alignment = taCenter
      Caption = 'Version 1.04'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 101
    end
    object Label10: TLabel
      Left = 1
      Top = 73
      Width = 431
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Front end to the TDiscImage Delphi class component'
      ExplicitWidth = 251
    end
    object Label14: TLabel
      Left = 1
      Top = 86
      Width = 431
      Height = 19
      Align = alTop
      Alignment = taCenter
      Caption = 'http://www.geraldholdsworth.co.uk'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 296
    end
  end
end
