--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Text_IO;

with Uinput;  use Uinput;

package body Injector is

   task body Injector_Task is
      Uinput_FID  : K_File_ID_T;
      IE_Time     : K_Timeval_T;  --  defaults to zeroes
      Key_IE      : K_Input_Event_T;
      Report_IE   : constant K_Input_Event_T := (Time     => IE_Time,
                                                 IE_Type  => EV_SYN,
                                                 IE_Code  => SYN_REPORT,
                                                 IE_Value => 0);
      Result      : K_Int_T;
   begin
      accept Start do
         K_Open (Uinput_FID, Linux_Path, O_WRONLY + O_NONBLOCK);
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
                  Key_IE.IE_Code := Char_To_K_Int (Keys (C));
                  Key_IE.IE_Value := 1;
                  --  IE_IO.Write (Uinput_File, Key_IE);
                  --  IE_IO.Write (Uinput_File, Report_IE);
                  Key_IE.IE_Value := 0;
                  --  IE_IO.Write (Uinput_File, Key_IE);
                  --  IE_IO.Write (Uinput_File, Report_IE);
               end loop;
            end Send;
         or
            accept Shutdown do
               K_Close (Result, Uinput_FID);
               return;
            end Shutdown;
         or
            terminate;
         end select;
      end loop;
   end Injector_Task;

end Injector;