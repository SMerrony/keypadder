--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2023 Stephen Merrony

with Ada.Text_IO;   use Ada.Text_IO;

with TOML;
with TOML.File_IO;

package body Config is

   procedure Load_Config_File (Filename : String; Verbose : Boolean := False) is
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
                  Keypadder_Conf.Port := Port_T (TOML.As_Integer (TOML.Get (Keypadder_Table, "port")));
                  if Verbose then
                     Put_Line ("Port:" & Keypadder_Conf.Port'Image);
                  end if;
               end;
            elsif To_String (TK) = "tab" then
               declare
                  Tabs_Array : constant TOML.TOML_Value := TOML.Get (Toml_Parse_Result.Value, "tab");
                  Tabs_Count : constant Natural         := TOML.Length (Tabs_Array);
                  Tab        : TOML.TOML_Value;

               begin
                  if Tabs_Count = 0 then
                     raise Incomplete_Configuration with "you must configure at least one Tab";
                  end if;
                  for T in 1 .. Tabs_Count loop
                     Tab_Ix := Tab_Ix + 1;
                     Tab := TOML.Item (Tabs_Array, T);
                     Tabs (T).Label   := TOML.As_Unbounded_String (TOML.Get (Tab, "label"));
                     Tabs (T).Columns := Natural (TOML.As_Integer (TOML.Get (Tab, "cols")));
                     --  Put_Line ("Tab defined:"  & TOML.Dump_As_String (Tab));
                     if Verbose then
                        Put_Line ("Tab: " & To_String (Tabs (T).Label) &
                                  " with:" & Tabs (T).Columns'Image & " columns");
                     end if;
                     declare
                        Keys_Array : constant TOML.TOML_Value := TOML.Get (Tab, "keys");
                        Key_Table  : TOML.TOML_Value;
                     begin
                        Put_Line ("Keys defined:"  & TOML.Length (Keys_Array)'Image);
                        Tabs (Tab_Ix).Keys_Count := TOML.Length (Keys_Array);
                        for K in 1 .. TOML.Length (Keys_Array) loop
                           Key_Table := TOML.Item (Keys_Array, K);
                           Tabs (Tab_Ix).Keys (K).Label := TOML.As_Unbounded_String (TOML.Get (Key_Table, "label"));
                           if TOML.Has (Key_Table, "send") then
                              Tabs (Tab_Ix).Keys (K).Send := TOML.As_Unbounded_String (TOML.Get (Key_Table, "send"));
                           end if;
                           if TOML.Has (Key_Table, "colspan") then
                              Tabs (Tab_Ix).Keys (K).Colspan := Natural (TOML.As_Integer (TOML.Get (Key_Table, "colspan")));
                           end if;
                           if TOML.Has (Key_Table, "rowspan") then
                              Tabs (Tab_Ix).Keys (K).Rowspan := Natural (TOML.As_Integer (TOML.Get (Key_Table, "rowspan")));
                           end if;
                           if Verbose then
                              Put_Line ("Key No. " & K'Image & " is: " & To_String (Tabs (Tab_Ix).Keys (K).Label));
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
   end Load_Config_File;

end Config;