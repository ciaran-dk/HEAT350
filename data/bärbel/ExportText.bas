Attribute VB_Name = "ExportText"
Option Explicit

Sub ExportText()

  Dim fso, fo, f
  Dim sFolder, sFile
  Dim wb, ws, ft

  Set fso = CreateObject("Scripting.FileSystemObject")
  sFolder = "C:\Data\GitHub\HEAT_100yrs\bärbel"
  Set fo = fso.GetFolder(sFolder)
  
  For Each f In fo.Folders
    sFile = f.Name
    If LCase(Right(sFile, 5)) = ".xlsx" Then
      Set wb = Workbooks.Open(f.Name)
      wb.Close
  
    End If
  Next '
End Sub

