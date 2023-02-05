--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Text_IO;

with Uinput;      use Uinput;

package body Injector is

   task body Injector_Task is
      Uinput_FID  : K_File_ID_T;
      Usetup      : K_Uinput_Setup_T;
      Result      : K_Int_T;
      Key_Val     : K_Int_T := 1; --  Key 0 is 'reserved' by the USB standard
      Request     : K_Ioctl_ID_T;

   begin
      accept Start do
         Uinput_FID := K_Open (Linux_Path, O_WRONLY + O_NONBLOCK);
         if Uinput_FID = -1 then
            Ada.Text_IO.Put_Line ("ERROR: Could not open " & Linux_Path);
            raise Cannot_Open;
         end if;

         --  set up for keyboard events
         Request := IOW (UI_SET_EVBIT, 4);
         Result := K_IOCTL_3_Arg (Uinput_FID, Request, K_Long_T (EV_KEY));
         if Result = -1 then
            C_Perror ("UI_SET_EVBIT failed...");
            raise IOCTL_Error with "UI_SET_EVBIT failed";
         end if;

         --  enable all possible (1 .. 127) key values
         loop
            Result := K_IOCTL_3_Arg (Uinput_FID, IOW (UI_SET_KEYBIT, 4), K_Long_T (Key_Val));
            if Result = -1 then
               Ada.Text_IO.Put_Line ("ERROR: Could not SET_KEYBIT");
               raise IOCTL_Error with "Could not SET_KEYBIT";
            end if;
            Key_Val := Key_Val + 1;
            exit when Key_Val = 128;
         end loop;

         --  configure our dummy USB device...
         Usetup.ID.K_Bustype := BUS_USB;
         Usetup.ID.K_Vendor  := 16#1234#; --  sample vendor
         Usetup.ID.K_Product := 16#5678#; --  sample K_Product
         Usetup.Name (1 .. 9) := "Keypadder";
         Usetup.Name (10 .. Uinput_Max_Name_Size) := (others => Character'Val (0));
         Request := IOW (UI_DEV_SETUP, Usetup'Size / 8);
         Result := K_IOCTL_Uinput_Setup (Uinput_FID, Request, Usetup);
         if Result = -1 then
            C_Perror ("UI_DEV_SETUP failed...");
            raise IOCTL_Error with "UI_DEV_SETUP failed";
         end if;

         --  create the dummy device
         Request := IO (UI_DEV_CREATE, 0);
         Result := K_IOCTL_2_Arg (Uinput_FID, Request);
         if Result = -1 then
            C_Perror ("UI_DEV_CREATE failed...");
            raise IOCTL_Error with "UI_DEV_CREATE failed";
         end if;
      end Start;

      loop
         select
            accept Send (Keys : String) do
               Ada.Text_IO.Put_Line ("Injector got Send request for: " & Keys);
               for C in Keys'Range loop
                  --  Key press, report the event, send key release, and report again
                  Ada.Text_IO.Put_Line ("Emitting character: " & Keys (C));
                  Emit (Uinput_FID, EV_KEY, Char_To_K_U16 (Keys (C)), 1);
                  Emit (Uinput_FID, EV_SYN, SYN_REPORT, 0);
                  Emit (Uinput_FID, EV_KEY, Char_To_K_U16 (Keys (C)), 0);
                  Emit (Uinput_FID, EV_SYN, SYN_REPORT, 0);
               end loop;
            end Send;
         or
            accept Shutdown do
               Result := K_Close (Uinput_FID);
               return;
            end Shutdown;
         or
            terminate;
         end select;
      end loop;
   end Injector_Task;

end Injector;