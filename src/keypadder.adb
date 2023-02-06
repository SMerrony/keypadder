--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with AWS.Server;
with AWS.Server.Status;

with GNAT.OS_Lib;

with Config;
with Frontend;
with Injector;
with Keys;

procedure Keypadder is

   App_SemVer : constant String := "0.1.0";  --  TODO Update App_SemVer for each release

   Arg_Ix     : Natural := 1;
   Config_Arg : Unbounded_String := Null_Unbounded_String;
   Verbose    : Boolean := False;

   WS : AWS.Server.HTTP;

begin

   Keys.Setup_Key_Map;

   while Arg_Ix <= Argument_Count loop
      if Argument (Arg_Ix) = "-V" or else Argument (Arg_Ix) = "-version" then
         Ada.Text_IO.Put_Line ("keypadder version: " & App_SemVer);
         goto FINISHED;
      elsif Argument (Arg_Ix) = "-v" or else Argument (Arg_Ix) = "-verbose" then
         Verbose := True;
      elsif Argument (Arg_Ix) = "-h" or else Argument (Arg_Ix) = "--help" then
         Put_Line ("Usage of keypadder:");
         Put_Line ("  --config=<config-file>  Configuration file for keypadder (required)");
         Put_Line ("  --dumpkeys              List all defined key mnemonics");
         Put_Line ("  -h | -help              This help");
         Put_Line ("  -V | -version           Show the version of keypadder and exit");
         Put_Line ("  -v | -verbose           Show lots of detail when running");
         goto FINISHED;
      elsif Argument (Arg_Ix)'Length > 9 and then Argument (Arg_Ix)(1 .. 9) = "--config=" then
         Config_Arg := To_Unbounded_String (Argument (Arg_Ix)(10 .. Argument (Arg_Ix)'Length));
      elsif Argument (Arg_Ix) = "--dumpkeys" then
         Keys.List_All_Keys;
         goto FINISHED;
      end if;
      Arg_Ix := Arg_Ix + 1;
   end loop;

   if Config_Arg = Null_Unbounded_String then
      Put_Line ("ERROR: You must specify a configuration file.  Use '-h' for help.");
      goto FINISHED;
   end if;

   if not Ada.Directories.Exists (To_String (Config_Arg)) then
      Put_Line ("ERROR: Configuration file: " & To_String (Config_Arg) & " does not exist.");
      goto FINISHED;
   end if;

   if not Config.Load_Config_File (To_String (Config_Arg), Verbose) then
      goto FINISHED;
   end if;

   Injector.Injector_Task.Start;

   AWS.Server.Start (WS,
                     Name => "Keypadder Server",
                     Callback => Frontend.Request_CB'Access,
                     Max_Connection => 3,
                     Port => Integer (Config.Conf.Keypadder_Conf.Port));
   Put_Line ("Server is running on " & AWS.Server.Status.Local_URL (WS));
   loop
      delay 3.0;
      exit when Frontend.Shutting_Down;
   end loop;
   AWS.Server.Shutdown (WS);

<<FINISHED>>
   GNAT.OS_Lib.OS_Exit (0);
end Keypadder;