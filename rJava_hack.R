# If rJava can't load properly
library(rJava)

# ...it might be because the environment path is not pointing to the real Java path
# Get the path by going:
Sys.getenv("Path")

# There will be a bit in there like 
#   C:\\Program Files \\Java\\jre1.8.0_112\\bin\\server\\;
# where the last three digits of the 'jre' subfolder might be outdated.  Fix this to be in correct subfolder of C:\Program Files\Java
x = "C:\\Program Files \\R\\R-3.3.1\\bin\\x64;C:\\Program Files(x86) \\Microsoft Visual Studio 14.0 \\Common7\\IDE\\CommonExtensions\\Microsoft\\TeamFoundation\\Team Explorer \\NativeBinaries\\x86;C:\\ProgramData\\Oracle\\Java\\javapath;C:\\Program Files(x86) \\Python35-32\\Scripts\\;C:\\Program Files(x86) \\Python35-32\\;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files(x86) \\Windows Kits \\8.1\\Windows Performance Toolkit \\;C:\\Program Files \\Microsoft SQL Server \\110\\Tools\\Binn\\;C:\\Program Files(x86) \\Microsoft SQL Server \\110\\Tools\\Binn\\ManagementStudio\\;C:\\Program Files(x86) \\Microsoft SQL Server \\110\\Tools\\Binn\\;C:\\Program Files(x86) \\Microsoft SQL Server \\110\\DTS\\Binn\\;C:\\Program Files(x86) \\Microsoft SDKs \\TypeScript\\1.0\\;C:\\Program Files \\Microsoft SQL Server \\120\\Tools\\Binn\\;C:\\Program Files \\Microsoft\\Web Platform Installer \\;C:\\Program Files \\Java\\jre1.8.0_112\\bin\\server\\;C:\\Program Files(x86) \\Microsoft SQL Server \\Client SDK \\ODBC\\130\\Tools\\Binn\\;C:\\Program Files(x86) \\Microsoft SQL Server \\130\\Tools\\Binn\\;C:\\Program Files(x86) \\Microsoft SQL Server \\130\\DTS\\Binn\\;C:\\Program Files(x86) \\Microsoft SQL Server \\130\\Tools\\Binn\\ManagementStudio\\;C:\\Program Files \\Git\\cmd;C:\\Users\\alebel\\.dnx\\bin;C:\\Program Files \\Microsoft DNX \\Dnvm\\;C:\\Program Files \\Microsoft SQL Server \\130\\Tools\\Binn\\;C:\\Perl\\c\\bin;C:\\Perl\\perl\\site\\bin;C:\\Perl\\perl\\bin"

# Similarly, you might want to edit the Windows environment variable.  Do this going WINDOWS + X key and select "Command Prompt (Admin)" 
# from the menu, and typing 
#
#   rundll32.exe sysdm.cpl,EditEnvironmentVariables
#
# and editing the PATH variable.  See http://winaero.com/blog/how-to-edit-environment-variables-quickly-in-windows-8-1-and-windows-8/

# Set the path environment once your satisfied the above value for x is correct
Sys.setenv(PATH = x)

# You might need to turn Visual Studio off and on again to do this right.