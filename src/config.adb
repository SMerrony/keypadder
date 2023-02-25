--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;

with TOML;              use  TOML;
with TOML.File_IO;

with Keys;

package body Config is

   function Decode_Send_String (User_String : Unbounded_String) return Event_Vectors.Vector is
      use Event_Vectors;
      use Keys;
      Decoded    : Vector;
      Words      : Slice_Set;
   begin
      Create (Words, To_String (User_String), ",", Multiple);
      for Word of Words loop
         if Word'Length > 15 and then Head (Word, 15) = "Ctrl+Alt+Shift+" then
            Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
            Decoded.Append ((Down, Keys_M ("LEFTALT")));
            Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
            if Keys_M.Contains (Word (Word'First + 15 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 15 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 15 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
               Decoded.Append ((Up, Keys_M ("LEFTALT")));
               Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word'Length > 11 and then Head (Word, 11) = "Ctrl+Shift+" then
            Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
            Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
            if Keys_M.Contains (Word (Word'First + 11 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 11 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 11 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
               Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word'Length > 9 and then Head (Word, 9) = "Ctrl+Alt+" then
            Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
            Decoded.Append ((Down, Keys_M ("LEFTALT")));
            if Keys_M.Contains (Word (Word'First + 9 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 9 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 9 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTALT")));
               Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word'Length > 6 and then Head (Word, 6) = "Shift+" then
            --  Shifted word...
            Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
            if Keys_M.Contains (Word (Word'First + 6 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 6 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 6 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word'Length > 5 and then Head (Word, 5) = "Ctrl+" then
            --  Controlled word...
            Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
            if Keys_M.Contains (Word (Word'First + 5 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 5 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 5 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word'Length > 4 and then Head (Word, 4) = "Alt+" then
            --  Controlled word...
            Decoded.Append ((Down, Keys_M ("LEFTALT")));
            if Keys_M.Contains (Word (Word'First + 4 .. Word'Last)) then
               Decoded.Append ((Down, Keys_M (Word (Word'First + 4 .. Word'Last))));
               Decoded.Append ((Up,   Keys_M (Word (Word'First + 4 .. Word'Last))));
               Decoded.Append ((Up, Keys_M ("LEFTALT")));
            else
               raise Unknown_Key with Word;
            end if;
         elsif Word (Word'First) = '@' then
            --  Unicode prefix...
            Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
            Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
            Decoded.Append ((Down, Keys_M ("U")));
            Decoded.Append ((Up, Keys_M ("U")));
            Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
            Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
         else
            if Keys_M.Contains (Word) then
               --  Normal word...
               Decoded.Append ((Down, Keys_M (Word)));
               Decoded.Append ((Up,   Keys_M (Word)));
            else
               raise Unknown_Key with Word;
            end if;
         end if;
      end loop;
      return Decoded;

   exception
      when Error : Unknown_Key =>
         Put_Line ("Error loading configuration file...");
         Put_Line ("Unknown key mnemonic: " & Exception_Message (Error));
         GNAT.OS_Lib.OS_Exit (-1);
      when Error : others =>
         Put_Line ("Error loading configuration file...");
         Put_Line (Exception_Message (Error));
         GNAT.OS_Lib.OS_Exit (-1);
   end Decode_Send_String;

   function Parse_And_Check_Config (Filename : String) return Read_Result is
      Parse_Result : Read_Result;
   begin
      Parse_Result := File_IO.Load_File (Filename);
      if not Parse_Result.Success then
         raise Could_Not_Parse with Filename & ":"
           & Parse_Result.Location.Line'Image & ":"
           & Parse_Result.Location.Column'Image & ": "
           & To_String (Parse_Result.Message);
      end if;

      --  Some sanity checking...
      declare
         Val, Res : TOML_Value;
      begin
         --  Is there a [keypadder] section?
         Val :=  Get_Or_Null (Parse_Result.Value, "keypadder");
         if Val.Is_Null then
            raise Incomplete_Configuration with "Missing [keypadder] section";
         end if;
         --  Is the port specified?
         Res := Get_Or_Null (Val, "port");
         if Res.Is_Null then
            raise Incomplete_Configuration with "Missing 'port' setting";
         end if;
         --  Is there at least one [tab] section?
         Val :=  Get_Or_Null (Parse_Result.Value, "tab");
         if Val.Is_Null then
            raise Incomplete_Configuration with "Missing [[tab]] section";
         end if;
      end;
      return Parse_Result;

   exception

      when Error : others =>
         Put_Line ("Error loading configuration file: " & Filename);
         Put_Line (Exception_Message (Error));
         GNAT.OS_Lib.OS_Exit (-1);
   end Parse_And_Check_Config;

   function Load_Config_File (Filename : String; Verbose : Boolean := False)
                             return Boolean is
      Toml_Parse_Result : constant Read_Result := Parse_And_Check_Config (Filename);
      Top_Keys : constant Key_Array := Toml_Parse_Result.Value.Keys;
      Tabs_Count : Positive;
   begin
      for TK of Top_Keys loop
         if Verbose then
            Put_Line ("Configuration for " & To_String (TK) & " is...");
         end if;

         --  kepadder section
         if To_String (TK) = "keypadder" then
            declare
               Keypadder_Table : constant TOML_Value := Get (Toml_Parse_Result.Value, "keypadder");
            begin
               Conf.Keypadder_Conf.Port := Port_T (As_Integer (Get (Keypadder_Table, "port")));
               if Verbose then
                  Put_Line ("Port:" & Conf.Keypadder_Conf.Port'Image);
               end if;
               if Keypadder_Table.Has ("tabswitch") then
                  declare
                     Ts : constant String := As_String (Get (Keypadder_Table, "tabswitch"));
                  begin
                     if Ts = "dropdown" then
                        Conf.Keypadder_Conf.Tabswitch := Dropdown;
                     elsif Ts = "tabs" then
                        Conf.Keypadder_Conf.Tabswitch := Tabs;
                     else
                        raise Invalid_Value with "Unknown tabswitch value";
                     end if;
                  end;
               end if;
            end;

         --  tabs section
         elsif To_String (TK) = "tab" then
            declare
               Tabs_Array : constant TOML_Value := Get (Toml_Parse_Result.Value, "tab");
               Tab        : TOML_Value;
               New_Tab    : Tab_T;
            begin
               Tabs_Count := Length (Tabs_Array);
               for T in 1 .. Tabs_Count loop
                  Tab := Item (Tabs_Array, T);
                  New_Tab.Label   := As_Unbounded_String (Get (Tab, "label"));
                  New_Tab.Columns := Natural (As_Integer (Get (Tab, "cols")));
                  if Has (Tab, "fontsize") then
                     New_Tab.Fontsize := As_Unbounded_String (Get (Tab, "fontsize"));
                  else
                     New_Tab.Fontsize := Null_Unbounded_String;
                  end if;
                  --  Put_Line ("Tab defined:"  & Dump_As_String (Tab));
                  if Verbose then
                     Put_Line ("Tab: " & To_String (New_Tab.Label) &
                                 " with:" & New_Tab.Columns'Image & " columns");
                  end if;
                  declare
                     Keys_Array : constant TOML_Value := Get (Tab, "keys");
                     Key_Table  : TOML_Value;
                     Blank      : constant Unbounded_String := To_Unbounded_String (" ");
                     New_Key    : Key_T;
                  begin
                     if Verbose then
                        Put_Line ("Keys defined:"  & Length (Keys_Array)'Image);
                     end if;
                     for K in 1 .. Length (Keys_Array) loop
                        Key_Table := Item (Keys_Array, K);

                        New_Key.Label := As_Unbounded_String (Get (Key_Table, "label"));
                        if New_Key.Label = "BLANK" then
                           New_Key.Label := Blank;
                        end if;

                        if Has (Key_Table, "send") then
                           New_Key.Send        := As_Unbounded_String (Get (Key_Table, "send"));
                           New_Key.Send_Events := Decode_Send_String (New_Key.Send);
                        elsif New_Key.Label = Blank then
                           New_Key.Send        := Null_Unbounded_String;
                           New_Key.Send_Events := Event_Vectors.Empty_Vector;
                        else
                           raise Incomplete_Configuration with "Missing 'send' item for label: " & To_String (New_Key.Label);
                        end if;

                        if Has (Key_Table, "colspan") then
                           New_Key.Colspan := Natural (As_Integer (Get (Key_Table, "colspan")));
                        else
                           New_Key.Colspan := 0;
                        end if;

                        if Has (Key_Table, "rowspan") then
                           New_Key.Rowspan := Natural (As_Integer (Get (Key_Table, "rowspan")));
                        else
                           New_Key.Rowspan := 0;
                        end if;

                        if Has (Key_Table, "bg") then
                           New_Key.Bg_Colour := As_Unbounded_String (Get (Key_Table, "bg"));
                        else
                           New_Key.Bg_Colour := Null_Unbounded_String;
                        end if;

                        New_Tab.Keys.Append (New_Key);
                        if Verbose then
                           Put_Line ("Key No. " & K'Image & " is: " & To_String (New_Key.Label));
                        end if;
                     end loop;
                  end;
                  Conf.Tabs.Append (New_Tab);
                  New_Tab.Keys := Key_Vectors.Empty_Vector;
               end loop; --  tabs
            end;
         else
            raise Unknown_Configuration_Item with To_String (TK);
         end if;

      end loop; -- Top_Keys

      if Conf.Keypadder_Conf.Tabswitch = Unset then
         if Tabs_Count <= Default_Max_Tabbed then
            Conf.Keypadder_Conf.Tabswitch := Tabs;
         else
            Conf.Keypadder_Conf.Tabswitch := Dropdown;
         end if;
      end if;
      return True;

   exception
      when Error : others =>
         Put_Line ("Error loading configuration file: " & Filename);
         Put_Line (Exception_Message (Error));
         return False;
   end Load_Config_File;

end Config;
