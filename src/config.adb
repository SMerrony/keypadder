--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.String_Split; use GNAT.String_Split;

with TOML;              use TOML;
with TOML.File_IO;

with Keys;

package body Config is

   function Decode_Send_String (User_String : Unbounded_String) return Vector is
      use Keys;
      Decoded    : Vector;
      Words      : Slice_Set;
   begin
      Create (Words, To_String (User_String), ",", Multiple);
      for Word_Num in 1 .. Slice_Count (Words) loop
         declare
            Word : constant String := Slice (Words, Word_Num);
         begin
            if Word'Length > 2 and then Word (Word'First) = '!' and then Word (Word'First + 1) = '^' then
               Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
               Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
               if Keys_M.Contains (Word (Word'First + 2 .. Word'Last)) then
                  Decoded.Append ((Down, Keys_M (Word (Word'First + 2 .. Word'Last))));
                  Decoded.Append ((Up,   Keys_M (Word (Word'First + 2 .. Word'Last))));
                  Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
                  Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
               else
                  raise Unknown_Key with Word;
               end if;
            elsif Word (Word'First) = '^' then
               --  Shifted word...
               Decoded.Append ((Down, Keys_M ("LEFTSHIFT")));
               if Keys_M.Contains (Word (Word'First + 1 .. Word'Last)) then
                  Decoded.Append ((Down, Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up,   Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up, Keys_M ("LEFTSHIFT")));
               else
                  raise Unknown_Key with Word;
               end if;
            elsif Word (Word'First) = '!' then
               --  Controlled word...
               Decoded.Append ((Down, Keys_M ("LEFTCTRL")));
               if Keys_M.Contains (Word (Word'First + 1 .. Word'Last)) then
                  Decoded.Append ((Down, Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up,   Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up, Keys_M ("LEFTCTRL")));
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
         end;
      end loop;
      return Decoded;
   end Decode_Send_String;

   function Load_Config_File (Filename : String; Verbose : Boolean := False)
                             return Boolean is
      Toml_Parse_Result : Read_Result;
   begin
      Toml_Parse_Result := File_IO.Load_File (Filename);
      if not Toml_Parse_Result.Success then
         raise Could_Not_Parse with Filename & ":"
           & Toml_Parse_Result.Location.Line'Image & ":"
           & Toml_Parse_Result.Location.Column'Image & ": "
           & To_String (Toml_Parse_Result.Message);
      end if;

      declare
         Top_Keys : constant Key_Array := Toml_Parse_Result.Value.Keys;
         Tab_Ix   : Natural := 0;
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
                           raise Invalid_Value with "unknown tabswitch value";
                        end if;
                     end;
                  end if;
               end;

            --  tabs section
            elsif To_String (TK) = "tab" then
               declare
                  Tabs_Array : constant TOML_Value := Get (Toml_Parse_Result.Value, "tab");
                  Tab        : TOML_Value;
               begin
                  Conf.Tabs_Count := Length (Tabs_Array);
                  if Conf.Tabs_Count = 0 then
                     raise Incomplete_Configuration with "you must configure at least one Tab";
                  end if;
                  for T in 1 .. Conf.Tabs_Count loop
                     Tab_Ix := Tab_Ix + 1;
                     Tab := Item (Tabs_Array, T);
                     Conf.Tabs (T).Label   := As_Unbounded_String (Get (Tab, "label"));
                     Conf.Tabs (T).Columns := Natural (As_Integer (Get (Tab, "cols")));
                     --  Put_Line ("Tab defined:"  & Dump_As_String (Tab));
                     if Verbose then
                        Put_Line ("Tab: " & To_String (Conf.Tabs (T).Label) &
                                  " with:" & Conf.Tabs (T).Columns'Image & " columns");
                     end if;
                     declare
                        Keys_Array : constant TOML_Value := Get (Tab, "keys");
                        Key_Table  : TOML_Value;
                        Blank      : constant Unbounded_String := To_Unbounded_String (" ");
                     begin
                        if Verbose then
                           Put_Line ("Keys defined:"  & Length (Keys_Array)'Image);
                        end if;
                        Conf.Tabs (Tab_Ix).Keys_Count := Length (Keys_Array);
                        for K in 1 .. Length (Keys_Array) loop
                           Key_Table := Item (Keys_Array, K);
                           Conf.Tabs (Tab_Ix).Keys (K).Label := As_Unbounded_String (Get (Key_Table, "label"));
                           if Conf.Tabs (Tab_Ix).Keys (K).Label = "BLANK" then
                              Conf.Tabs (Tab_Ix).Keys (K).Label := Blank;
                           end if;
                           if Has (Key_Table, "send") then
                              Conf.Tabs (Tab_Ix).Keys (K).Send := As_Unbounded_String (Get (Key_Table, "send"));
                              Conf.Tabs (Tab_Ix).Keys (K).Send_Events := Decode_Send_String (Conf.Tabs (Tab_Ix).Keys (K).Send);
                           end if;
                           if Has (Key_Table, "colspan") then
                              Conf.Tabs (Tab_Ix).Keys (K).Colspan := Natural (As_Integer (Get (Key_Table, "colspan")));
                           end if;
                           if Has (Key_Table, "rowspan") then
                              Conf.Tabs (Tab_Ix).Keys (K).Rowspan := Natural (As_Integer (Get (Key_Table, "rowspan")));
                           end if;
                           if Verbose then
                              Put_Line ("Key No. " & K'Image & " is: " & To_String (Conf.Tabs (Tab_Ix).Keys (K).Label));
                           end if;
                        end loop;
                     end;
                  end loop; --  tabs
               end;
            else
               raise Unknown_Configuration_Item with To_String (TK);
            end if;

         end loop; -- Top_Keys

         if Conf.Keypadder_Conf.Tabswitch = Unset then
            if Conf.Tabs_Count <= Default_Max_Tabbed then
               Conf.Keypadder_Conf.Tabswitch := Tabs;
            else
               Conf.Keypadder_Conf.Tabswitch := Dropdown;
            end if;
         end if;
      end;
      return True;
   exception
      when Error : others =>
         Put_Line ("Error loading configuration file: " & Filename);
         Put_Line (Exception_Information (Error));
         return False;
   end Load_Config_File;

end Config;
