--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

package Frontend is

   Shutting_Down  : Boolean := False;
   Main_Page_HTML : Unbounded_String;

   function Request_CB (Request : AWS.Status.Data) return AWS.Response.Data;
   --  Main handler for HTTP requests.

   procedure Build_Main_Page;
   --  Transform the configuration into our web page.
   --  Must be called after our configuration is loaded.

private

   procedure Decode_And_Send_Key (Key_ID : String; Tab : out Positive);
   --  Find the key from the ID string and send the
   --  associated data via the Injector.
   --  The ID arrives in the form "key_t1i8" which indcates
   --  Tab #1, Key #8.

end Frontend;