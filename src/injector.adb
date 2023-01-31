--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Sequential_IO;
with Ada.Text_IO;

package body Injector is

   --  package Char_IO is new Ada.Sequential_IO (Character);
   package IE_IO is new Ada.Sequential_IO (Input_Event_T);

   task body Injector_Task is
      Uinput_File : IE_IO.File_Type;
      IE_Time     : Timeval_T;  --  defaults to zeroes
      Key_IE      : Input_Event_T;
      Report_IE   : constant Input_Event_T := (Time     => IE_Time,
                                               IE_Type  => EV_SYN,
                                               IE_Code  => SYN_REPORT,
                                               IE_Value => 0);
   begin
      accept Start do
         IE_IO.Open (Uinput_File, IE_IO.Out_File, Linux_Path);
         --  populate the unchanging fields in the Key_IE
         Key_IE.Time := IE_Time;
         Key_IE.IE_Type := EV_KEY;
      end Start;
      loop
         select
            accept Send (Keys : String) do
               Ada.Text_IO.Put_Line ("Injector got Send request for: " & Keys);
               for C in Keys'Range loop
                  --  Key press, report the event, send key release, and report again
                  Key_IE.IE_Code := Char_To_U16 (Keys (C));
                  Key_IE.IE_Value := 1;
                  IE_IO.Write (Uinput_File, Key_IE);
                  IE_IO.Write (Uinput_File, Report_IE);
                  Key_IE.IE_Value := 0;
                  IE_IO.Write (Uinput_File, Key_IE);
                  IE_IO.Write (Uinput_File, Report_IE);
               end loop;
            end Send;
         or
            accept Shutdown do
               IE_IO.Close (Uinput_File);
               return;
            end Shutdown;
         or
            terminate;
         end select;
      end loop;
   end Injector_Task;

end Injector;