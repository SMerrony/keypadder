--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText:  Copyright 2023 Stephen Merrony

with AWS.Response;
with AWS.Status;

package Frontend is

   Shutting_Down  : Boolean := False;

   function Request_CB (Request : AWS.Status.Data) return AWS.Response.Data;
   --  Main handler for HTTP requests.

private

   function Build_Main_Page (Active_Tab : Positive) return String;
   --  Transform the configuration into our web page.
   --  Must be called after our configuration is loaded.

   procedure Send_Key (T, I : String);
   --  Send the associated keypress data via the Injector.

end Frontend;