--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNAT.String_Split; use GNAT.String_Split;

with TOML;
with TOML.File_IO;

with Keys;

package body Config is

   function Decode_Send_String (User_String : Unbounded_String) return Vector is
      Decoded    : Vector;
      Words      : Slice_Set;
   begin
      Create (Words, To_String (User_String), ",", Multiple);
      for Word_Num in 1 .. Slice_Count (Words) loop
         declare
            Word : constant String := Slice (Words, Word_Num);
         begin
            if Word (Word'First) = '^' then
               Decoded.Append ((Down, Keys.Keys_M ("LEFTSHIFT")));
               if Keys.Keys_M.Contains (Word (Word'First + 1 .. Word'Last)) then
                  Decoded.Append ((Down, Keys.Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up,   Keys.Keys_M (Word (Word'First + 1 .. Word'Last))));
                  Decoded.Append ((Up, Keys.Keys_M ("LEFTSHIFT")));
               else
                  raise Unknown_Key with Word;
               end if;
            else
               if Keys.Keys_M.Contains (Word) then
                  Put_Line ("Adding decoded conf: " & Word);
                  Decoded.Append ((Down, Keys.Keys_M (Word)));
                  Decoded.Append ((Up,   Keys.Keys_M (Word)));
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
      Toml_Parse_Result : TOML.Read_Result;
   begin
      Toml_Parse_Result := TOML.File_IO.Load_File (Filename);
      if not Toml_Parse_Result.Success then
         raise Could_Not_Parse with To_String (Toml_Parse_Result.Message);
      end if;

      declare
         Top_Keys : constant TOML.Key_Array := Toml_Parse_Result.Value.Keys;
         Tab_Ix   : Natural := 0;
      begin
         for TK of Top_Keys loop
            if Verbose then
               Put_Line ("Configuration for " & To_String (TK) & " is...");
            end if;

            if To_String (TK) = "keypadder" then
               declare
                  Keypadder_Table : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "keypadder");
               begin
                  Conf.Keypadder_Conf.Port := Port_T (TOML.As_Integer (TOML.Get (Keypadder_Table, "port")));
                  if Verbose then
                     Put_Line ("Port:" & Conf.Keypadder_Conf.Port'Image);
                  end if;
               end;
            elsif To_String (TK) = "tab" then
               declare
                  Tabs_Array : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "tab");
                  Tab        : TOML.TOML_Value;
               begin
                  Conf.Tabs_Count := TOML.Length (Tabs_Array);
                  if Conf.Tabs_Count = 0 then
                     raise Incomplete_Configuration with "you must configure at least one Tab";
                  end if;
                  for T in 1 .. Conf.Tabs_Count loop
                     Tab_Ix := Tab_Ix + 1;
                     Tab := TOML.Item (Tabs_Array, T);
                     Conf.Tabs (T).Label   := TOML.As_Unbounded_String (TOML.Get (Tab, "label"));
                     Conf.Tabs (T).Columns := Natural (TOML.As_Integer (TOML.Get (Tab, "cols")));
                     --  Put_Line ("Tab defined:"  & TOML.Dump_As_String (Tab));
                     if Verbose then
                        Put_Line ("Tab: " & To_String (Conf.Tabs (T).Label) &
                                  " with:" & Conf.Tabs (T).Columns'Image & " columns");
                     end if;
                     declare
                        Keys_Array : constant TOML.TOML_Value := TOML.Get (Tab, "keys");
                        Key_Table  : TOML.TOML_Value;
                        Blank      : constant Unbounded_String := To_Unbounded_String (" ");
                     begin
                        Put_Line ("Keys defined:"  & TOML.Length (Keys_Array)'Image);
                        Conf.Tabs (Tab_Ix).Keys_Count := TOML.Length (Keys_Array);
                        for K in 1 .. TOML.Length (Keys_Array) loop
                           Key_Table := TOML.Item (Keys_Array, K);
                           Conf.Tabs (Tab_Ix).Keys (K).Label := TOML.As_Unbounded_String (TOML.Get (Key_Table, "label"));
                           if Conf.Tabs (Tab_Ix).Keys (K).Label = "BLANK" then
                              Conf.Tabs (Tab_Ix).Keys (K).Label := Blank;
                           end if;
                           if TOML.Has (Key_Table, "send") then
                              Conf.Tabs (Tab_Ix).Keys (K).Send := TOML.As_Unbounded_String (TOML.Get (Key_Table, "send"));
                              Conf.Tabs (Tab_Ix).Keys (K).Send_Events := Decode_Send_String (Conf.Tabs (Tab_Ix).Keys (K).Send);
                           end if;
                           if TOML.Has (Key_Table, "colspan") then
                              Conf.Tabs (Tab_Ix).Keys (K).Colspan := Natural (TOML.As_Integer (TOML.Get (Key_Table, "colspan")));
                           end if;
                           if TOML.Has (Key_Table, "rowspan") then
                              Conf.Tabs (Tab_Ix).Keys (K).Rowspan := Natural (TOML.As_Integer (TOML.Get (Key_Table, "rowspan")));
                           end if;
                           if Verbose then
                              Put_Line ("Key No. " & K'Image & " is: " & To_String (Conf.Tabs (Tab_Ix).Keys (K).Label));
                           end if;
                        end loop;
                     end;
                  end loop;
               end;
            else
               raise Unknown_Configuration_Item with To_String (TK);
            end if;

         end loop;
      end;
      return True;
   exception
      when Error : others =>
         Put_Line ("Error loading configuration file: " & Filename);
         Put_Line (Exception_Information (Error));
         return False;
   end Load_Config_File;

end Config;