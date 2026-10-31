using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.IO.MemoryMappedFiles;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Net;

namespace FastDwgSearch
{
    class Program
    {
        static readonly Encoding Latin1 = Encoding.GetEncoding(28591);
        static readonly Encoding Utf8NoBom = new UTF8Encoding(false);
        static readonly Encoding Utf8Bom = new UTF8Encoding(true);

        static readonly Dictionary<char, int[]> VniVowelMap = new Dictionary<char, int[]>();
        public static readonly string CurrentVersion = "1.0.0";
        public static readonly string GitHubRepo = "datbt204-boop/CadaPro-CAD";

        public static bool IsNewerVersion(string remote, string local)
        {
            try
            {
                string[] rP = remote.Trim().TrimStart('v', 'V').Split('.');
                string[] lP = local.Trim().TrimStart('v', 'V').Split('.');
                for (int i = 0; i < Math.Max(rP.Length, lP.Length); i++)
                {
                    int rVal = i < rP.Length ? int.Parse(rP[i]) : 0;
                    int lVal = i < lP.Length ? int.Parse(lP[i]) : 0;
                    if (rVal > lVal) return true;
                    if (rVal < lVal) return false;
                }
                return false;
            }
            catch
            {
                return !string.Equals(remote.Trim(), local.Trim(), StringComparison.OrdinalIgnoreCase);
            }
        }

        public static string FetchRemoteVersion()
        {
            string[] urls = new string[] {
                "https://raw.githubusercontent.com/" + GitHubRepo + "/main/CADAPRO_CAD_GITHUB/version.txt",
                "https://raw.githubusercontent.com/" + GitHubRepo + "/main/version.txt"
            };
            foreach (var url in urls)
            {
                try
                {
                    var req = (HttpWebRequest)WebRequest.Create(url);
                    req.Timeout = 3000;
                    req.ReadWriteTimeout = 3000;
                    req.UserAgent = "CadaPro-CAD";
                    using (var resp = req.GetResponse())
                    using (var sr = new StreamReader(resp.GetResponseStream()))
                    {
                        string txt = sr.ReadToEnd().Trim();
                        if (!string.IsNullOrEmpty(txt)) return txt;
                    }
                }
                catch { }
            }
            return null;
        }

        public static void CheckUpdateSilent()
        {
            try
            {
                string remoteVer = FetchRemoteVersion();
                if (!string.IsNullOrEmpty(remoteVer) && IsNewerVersion(remoteVer, CurrentVersion))
                {
                    string noticePath = Path.Combine(WorkDir, "new_update_notice.txt");
                    File.WriteAllText(noticePath, remoteVer);
                }
            }
            catch { }
        }

        public static bool PerformDirectUpdate()
        {
            try
            {
                string[] files = new string[] { "TRACUU.LSP", "FastDwgSearch.exe", "GOPTO.LSP", "version.txt", "compile_cs.bat", "CAP_NHAT_TU_GITHUB.bat" };
                string rawRoot = "https://raw.githubusercontent.com/" + GitHubRepo + "/main";
                string appDir = AppDomain.CurrentDomain.BaseDirectory;
                string[] targetDirs = new string[] { appDir, @"C:\EXTRACUUAUTOCAD", @"D:\EXTRACUUAUTOCAD" };

                using (var wc = new WebClient())
                {
                    foreach (var f in files)
                    {
                        byte[] data = null;
                        try { data = wc.DownloadData(rawRoot + "/CADAPRO_CAD_GITHUB/" + f); }
                        catch
                        {
                            try { data = wc.DownloadData(rawRoot + "/" + f); } catch { }
                        }

                        if (data != null && data.Length > 0)
                        {
                            foreach (var dir in targetDirs)
                            {
                                if (Directory.Exists(dir))
                                {
                                    string targetPath = Path.Combine(dir, f);
                                    if (f.Equals("FastDwgSearch.exe", StringComparison.OrdinalIgnoreCase))
                                    {
                                        string newExe = Path.Combine(dir, "FastDwgSearch.new.exe");
                                        File.WriteAllBytes(newExe, data);
                                        try { File.Copy(newExe, targetPath, true); } catch { }
                                    }
                                    else
                                    {
                                        File.WriteAllBytes(targetPath, data);
                                    }
                                }
                            }
                        }
                    }
                }

                try
                {
                    string noticePath = Path.Combine(WorkDir, "new_update_notice.txt");
                    if (File.Exists(noticePath)) File.Delete(noticePath);
                }
                catch { }

                return true;
            }
            catch
            {
                return false;
            }
        }

        static readonly Dictionary<int, int> VniSingleMap = new Dictionary<int, int>();
        static readonly Dictionary<int, int> TcvnMap = new Dictionary<int, int>();
        static readonly Dictionary<int, int> Win1252Reverse = new Dictionary<int, int>();

        static Program()
        {
            InitTables();
        }

        static void InitTables()
        {
            // VNI vowel rows:
            // 0..4: sac, huyen, hoi, nga, nang
            // 5..10: hat(^), hat+sac, hat+huyen, hat+hoi, hat+nga, hat+nang
            // 11..16: breve((), breve+sac, breve+huyen, breve+hoi, breve+nga, breve+nang
            VniVowelMap['A'] = new int[] { 193, 192, 7842, 195, 7840, 194, 7844, 7846, 7848, 7850, 7852, 258, 7854, 7856, 7858, 7860, 7862 };
            VniVowelMap['a'] = new int[] { 225, 224, 7843, 227, 7841, 226, 7845, 7847, 7849, 7851, 7853, 259, 7855, 7857, 7859, 7861, 7863 };
            VniVowelMap['E'] = new int[] { 201, 200, 7866, 7868, 7864, 202, 7870, 7872, 7874, 7876, 7878, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['e'] = new int[] { 233, 232, 7867, 7869, 7865, 234, 7871, 7873, 7875, 7877, 7879, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['O'] = new int[] { 211, 210, 7886, 213, 7884, 212, 7888, 7890, 7892, 7894, 7896, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['o'] = new int[] { 243, 242, 7887, 245, 7885, 244, 7889, 7891, 7893, 7895, 7897, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['U'] = new int[] { 218, 217, 7910, 360, 7908, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['u'] = new int[] { 250, 249, 7911, 361, 7909, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['I'] = new int[] { 205, 204, 7880, 296, 7882, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['i'] = new int[] { 237, 236, 7881, 297, 7883, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['Y'] = new int[] { 221, 7922, 7926, 7928, 7924, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['y'] = new int[] { 253, 7923, 7927, 7929, 7925, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            VniVowelMap['\u01A0'] = new int[] { 7898, 7900, 7902, 7904, 7906, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; // Ơ
            VniVowelMap['\u01A1'] = new int[] { 7899, 7901, 7903, 7905, 7907, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; // ơ
            VniVowelMap['\u01AF'] = new int[] { 7912, 7914, 7916, 7918, 7920, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; // Ư
            VniVowelMap['\u01B0'] = new int[] { 7913, 7915, 7917, 7919, 7921, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; // ư

            // VNI 1-byte characters
            int[,] vni1 = new int[,] {
                {209, 272}, {241, 273},   // Đ, đ
                {205, 205}, {237, 237},   // Í, í
                {204, 204}, {236, 236},   // Ì, ì
                {198, 7880}, {230, 7881}, // Ỉ, ỉ
                {211, 296}, {243, 297},   // Ĩ, ĩ
                {210, 7882}, {242, 7883}, // Ị, ị
                {212, 416}, {244, 417},   // Ơ, ơ
                {214, 431}, {246, 432},   // Ư, ư
                {206, 7924}, {238, 7925}  // Ỵ, ỵ
            };
            for (int i = 0; i < vni1.GetLength(0); i++)
                VniSingleMap[vni1[i, 0]] = vni1[i, 1];

            // TCVN3 (ABC) + VHARIAL table
            int[,] tcvn = new int[,] {
                {161, 258}, {162, 194}, {163, 202}, {164, 212}, {165, 416}, {166, 431}, {167, 272},
                {168, 259}, {169, 226}, {170, 234}, {171, 244}, {172, 417}, {173, 432}, {174, 273},
                {181, 224}, {182, 7843}, {183, 227}, {184, 225}, {185, 7841},
                {187, 7857}, {188, 7859}, {189, 7861}, {190, 7855}, {198, 7863},
                {199, 7847}, {200, 7849}, {201, 7851}, {202, 7845}, {203, 7853},
                {204, 232}, {206, 7867}, {207, 7869}, {208, 233}, {209, 7865},
                {210, 7873}, {211, 7875}, {212, 7877}, {213, 7871}, {214, 7879},
                {215, 236}, {216, 7881}, {219, 297}, {220, 297}, {221, 237}, {222, 7883},
                {223, 242}, {225, 7887}, {226, 245}, {227, 243}, {228, 7885},
                {229, 7891}, {230, 7893}, {231, 7895}, {232, 7889}, {233, 7897},
                {234, 7901}, {235, 7903}, {236, 7905}, {237, 7899}, {238, 7907},
                {239, 249}, {241, 7911}, {242, 361}, {243, 250}, {244, 7909},
                {245, 7915}, {246, 7917}, {247, 7919}, {248, 7913}, {249, 7921},
                {250, 7923}, {251, 7927}, {252, 7929}, {253, 253}, {254, 7925},
                // Capital TCVN3 VHARIAL codes (128..159)
                {128, 192}, {129, 7842}, {130, 195}, {131, 193}, {132, 7840},
                {133, 7846}, {134, 7848}, {135, 7850}, {136, 7844}, {137, 7852},
                {138, 7856}, {139, 7858}, {140, 7860}, {141, 7854}, {142, 7862},
                {143, 200}, {144, 7866}, {145, 7868}, {146, 201}, {147, 7864},
                {148, 7872}, {149, 7874}, {150, 7876}, {151, 7870}, {152, 7878},
                {153, 204}, {154, 7880}, {155, 296}, {156, 205}, {157, 7882},
                {158, 210}, {159, 7886}
            };
            for (int i = 0; i < tcvn.GetLength(0); i++)
                TcvnMap[tcvn[i, 0]] = tcvn[i, 1];

            // Windows-1252 reverse map for 128..159
            int[,] w1252 = new int[,] {
                {8364, 128}, {8218, 130}, {402, 131}, {8222, 132}, {8230, 133},
                {8224, 134}, {8225, 135}, {710, 136}, {8240, 137}, {352, 138},
                {8249, 139}, {338, 140}, {381, 142}, {8216, 145}, {8217, 146},
                {8220, 147}, {8221, 148}, {8226, 149}, {8211, 150}, {8212, 151},
                {732, 152}, {8482, 153}, {353, 154}, {8250, 155}, {339, 156},
                {382, 158}, {376, 159}
            };
            for (int i = 0; i < w1252.GetLength(0); i++)
                Win1252Reverse[w1252[i, 0]] = w1252[i, 1];
        }

        // Strict case-sensitive VNI diacritic index:
        // Uppercase vowels ONLY match uppercase diacritics (isUpper=true);
        // Lowercase vowels ONLY match lowercase diacritics (isUpper=false).
        static int GetVniDiacriticIndexStrict(int d, bool isUpper)
        {
            if (isUpper)
            {
                switch (d)
                {
                    case 217: return 0;  // sac
                    case 216: return 1;  // huyen
                    case 219: return 2;  // hoi
                    case 213: return 3;  // nga
                    case 207: return 4;  // nang
                    case 194: return 5;  // ^
                    case 193: return 6;  // ^ + sac
                    case 192: return 7;  // ^ + huyen
                    case 196: return 8;  // ^ + hoi
                    case 195: return 9;  // ^ + nga
                    case 197: return 10; // ^ + nang
                    case 202: return 11; // (
                    case 201: return 12; // ( + sac
                    case 200: return 13; // ( + huyen
                    case 218: return 14; // ( + hoi
                    case 220: return 15; // ( + nga
                    case 203: return 16; // ( + nang
                    default: return -1;
                }
            }
            else
            {
                switch (d)
                {
                    case 249: return 0;  // sac
                    case 248: return 1;  // huyen
                    case 251: return 2;  // hoi
                    case 245: return 3;  // nga
                    case 239: return 4;  // nang
                    case 226: return 5;  // ^
                    case 225: return 6;  // ^ + sac
                    case 224: return 7;  // ^ + huyen
                    case 228: return 8;  // ^ + hoi
                    case 227: return 9;  // ^ + nga
                    case 229: return 10; // ^ + nang
                    case 234: return 11; // (
                    case 233: return 12; // ( + sac
                    case 232: return 13; // ( + huyen
                    case 250: return 14; // ( + hoi
                    case 252: return 15; // ( + nga
                    case 235: return 16; // ( + nang
                    default: return -1;
                }
            }
        }

        // Returns > 0 if (c1, c2) is a valid case-matched VNI 2-byte pair
        static int TryGetVniPairUni(int c1, int c2)
        {
            char baseChar;
            bool isUpper;
            switch (c1)
            {
                case 'A': baseChar = 'A'; isUpper = true; break;
                case 'E': baseChar = 'E'; isUpper = true; break;
                case 'I': baseChar = 'I'; isUpper = true; break;
                case 'O': baseChar = 'O'; isUpper = true; break;
                case 'U': baseChar = 'U'; isUpper = true; break;
                case 'Y': baseChar = 'Y'; isUpper = true; break;
                case 212: baseChar = '\u01A0'; isUpper = true; break; // Ơ
                case 214: baseChar = '\u01AF'; isUpper = true; break; // Ư
                case 'a': baseChar = 'a'; isUpper = false; break;
                case 'e': baseChar = 'e'; isUpper = false; break;
                case 'i': baseChar = 'i'; isUpper = false; break;
                case 'o': baseChar = 'o'; isUpper = false; break;
                case 'u': baseChar = 'u'; isUpper = false; break;
                case 'y': baseChar = 'y'; isUpper = false; break;
                case 244: baseChar = '\u01A1'; isUpper = false; break; // ơ
                case 246: baseChar = '\u01B0'; isUpper = false; break; // ư
                default: return 0;
            }
            int dIdx = GetVniDiacriticIndexStrict(c2, isUpper);
            if (dIdx < 0) return 0;
            int[] row;
            if (VniVowelMap.TryGetValue(baseChar, out row) && dIdx < row.Length)
                return row[dIdx];
            return 0;
        }

        static string ExpandHexAndPct(string s)
        {
            if (string.IsNullOrEmpty(s)) return "";
            StringBuilder sb = new StringBuilder(s.Length);
            int n = s.Length;
            int i = 0;
            while (i < n)
            {
                char c = s[i];
                if (c == '\\' && i + 6 < n && (s[i + 1] == 'U' || s[i + 1] == 'u') && s[i + 2] == '+')
                {
                    string hex = s.Substring(i + 3, 4);
                    int val;
                    if (int.TryParse(hex, NumberStyles.HexNumber, CultureInfo.InvariantCulture, out val) && val > 0)
                    {
                        sb.Append((char)val);
                        i += 7;
                        continue;
                    }
                }
                else if (c == '%' && i + 1 < n && s[i + 1] == '%')
                {
                    if (i + 4 < n && char.IsDigit(s[i + 2]) && char.IsDigit(s[i + 3]) && char.IsDigit(s[i + 4]))
                    {
                        int val = (s[i + 2] - '0') * 100 + (s[i + 3] - '0') * 10 + (s[i + 4] - '0');
                        if (val >= 32 && val <= 255)
                        {
                            sb.Append((char)val);
                            i += 5;
                            continue;
                        }
                    }
                    else if (i + 3 < n && char.IsDigit(s[i + 2]) && char.IsDigit(s[i + 3]))
                    {
                        int val = (s[i + 2] - '0') * 10 + (s[i + 3] - '0');
                        if (val >= 32 && val <= 99)
                        {
                            sb.Append((char)val);
                            i += 4;
                            continue;
                        }
                    }
                    else if (i + 2 < n)
                    {
                        char nx = char.ToUpperInvariant(s[i + 2]);
                        if (nx == 'C') { sb.Append('\u00D8'); i += 3; continue; }
                        if (nx == 'D') { sb.Append('\u00B0'); i += 3; continue; }
                        if (nx == 'P') { sb.Append('\u00B1'); i += 3; continue; }
                        if (nx == 'U' || nx == 'O' || nx == 'K') { i += 3; continue; }
                    }
                }
                sb.Append(c);
                i++;
            }
            return sb.ToString();
        }

        static string CleanMTextAndDetectInlineFont(string s, out string inlineMode)
        {
            inlineMode = null;
            if (string.IsNullOrEmpty(s)) return "";

            string up = s.ToUpperInvariant();
            if (up.Contains("\\F"))
            {
                if (up.Contains("\\F.VN") || up.Contains("\\FVNARIAL") || up.Contains("\\FVHARIAL") || up.Contains("\\FVNTIME") || up.Contains("\\FVKNT") || up.Contains("\\FTCVN"))
                    inlineMode = "TCVN";
                else if (up.Contains("\\FVNI") || up.Contains("\\FVTIMES") || up.Contains("\\FAVO") || up.Contains("\\FVHELVE") || up.Contains("\\FVAPTIM"))
                    inlineMode = "VNI";
            }

            StringBuilder sb = new StringBuilder(s.Length);
            int n = s.Length;
            int i = 0;
            while (i < n)
            {
                char c = s[i];
                if (c == '\\' && i + 1 < n)
                {
                    char rawNx = s[i + 1];
                    char nx = char.ToUpperInvariant(rawNx);
                    // Uppercase \P is MText newline; lowercase \p followed by ; (like \pxi-3,l3,t3;) is paragraph formatting!
                    if (rawNx == 'P')
                    {
                        sb.Append(" / ");
                        i += 2;
                        continue;
                    }
                    if (rawNx == 'p')
                    {
                        int semi = s.IndexOf(';', i + 2);
                        if (semi > i + 1 && semi - i <= 30)
                        {
                            i = semi + 1;
                            continue;
                        }
                        sb.Append(" / ");
                        i += 2;
                        continue;
                    }
                    if (nx == '~') { sb.Append(' '); i += 2; continue; }
                    if (nx == '\\' || nx == '{' || nx == '}') { sb.Append(s[i + 1]); i += 2; continue; }
                    if (nx == 'L' || nx == 'O' || nx == 'K') { i += 2; continue; }
                    if ("ACHWQTFS".IndexOf(nx) >= 0)
                    {
                        i += 2;
                        while (i < n && s[i] != ';')
                        {
                            if (nx == 'S') sb.Append(s[i]);
                            i++;
                        }
                        if (i < n && s[i] == ';') i++;
                        continue;
                    }
                }
                if (c != '{' && c != '}')
                    sb.Append(c);
                i++;
            }
            return sb.ToString();
        }

        static int NormByte(char ch)
        {
            int c = (int)ch;
            int rev;
            if (Win1252Reverse.TryGetValue(c, out rev))
                return rev;
            return c;
        }

        // Checks if a token/word has an unmistakable VNI pattern
        static bool WordHasStrongVni(string w)
        {
            int n = w.Length;
            for (int i = 0; i < n; i++)
            {
                int c1 = NormByte(w[i]);
                int c2 = (i + 1 < n) ? NormByte(w[i + 1]) : -1;

                // 1. ÖÔ (214+212 = ƯƠ) or öô (246+244 = ươ)
                if ((c1 == 214 && c2 == 212) || (c1 == 246 && c2 == 244))
                    return true;

                // 2. Ñ (209) at start of word or in uppercase word (ÑÌNH, ÑKL, CÑ)
                if (c1 == 209 && (i == 0 || (i > 0 && w[i - 1] >= 'A' && w[i - 1] <= 'Z')))
                    return true;
                // ñ (241) at start of word (ñình, ñường)
                if (c1 == 241 && i == 0)
                    return true;

                // 3. Valid VNI 2-byte pair
                if (c2 != -1 && TryGetVniPairUni(c1, c2) > 0)
                {
                    // Check for the ONLY ambiguity with Unicode/TCVN3:
                    // A) In Unicode: 'O' + À(192)/Á(193)/Ã(195) (as in HOÀNG, TOÀN, ĐOÀN, HOÁ, TOÁN)
                    //    or 'o' + à(224)/á(225)/ã(227) (as in hoàng, toàn, đoàn, hoá, toán)
                    if ((c1 == 'O' && (c2 == 192 || c2 == 193 || c2 == 195)) ||
                        (c1 == 'o' && (c2 == 224 || c2 == 225 || c2 == 227)))
                    {
                        // If preceded by U/u (e.g. QUOÁC, quoác), it is 100% VNI because UOA doesn't take acute/grave on A!
                        if (i > 0 && (w[i - 1] == 'U' || w[i - 1] == 'u'))
                            return true;
                        // Otherwise O+À/Á is ambiguous with Unicode HOÀNG/TOÁN, so don't trigger strong VNI by itself
                        continue;
                    }
                    // B) In TCVN3: 'u'(117) + û(251=ỷ in TCVN3, as in Thủy, ủy)
                    if (c1 == 'u' && c2 == 251)
                        continue;

                    return true;
                }
            }
            return false;
        }

        // Checks if a token/word has an unmistakable TCVN3 pattern
        static bool WordHasStrongTcvn(string w)
        {
            int n = w.Length;
            for (int i = 0; i < n; i++)
            {
                int c = NormByte(w[i]);
                // Unambiguous TCVN3 bytes that NEVER exist in Vietnamese Unicode:
                if ((c >= 128 && c <= 174) || (c >= 181 && c <= 190) ||
                    c == 198 || c == 199 || c == 203 ||
                    (c >= 206 && c <= 207) || (c >= 214 && c <= 216) ||
                    c == 219 || c == 220 || c == 222 || c == 223 ||
                    (c >= 228 && c <= 231) || c == 235 || (c >= 238 && c <= 241) ||
                    (c >= 246 && c <= 248) || c == 251 || c == 252 || c == 254)
                {
                    return true;
                }
                // Lowercase ASCII letter followed by uppercase Latin-1 (200..221) inside a word (e.g., NguyÔn, cÊp, đÊt, YÕn, kÕ, chÝnh)
                if (c >= 200 && c <= 221 && i > 0 && w[i - 1] >= 'a' && w[i - 1] <= 'z')
                {
                    return true;
                }
                // Phonotactically impossible in Vietnamese Unicode, 100% TCVN3:
                // 1) Grave/tilde/circumflex Latin-1 vowels (ã=227, è=232, ì=236, ò=242, ô=244, õ=245, ù=249) followed by stop consonants c, p, t
                //    (e.g., Quèc, tèt, thùc, lùc, lôc, môc, phôc, gãc, mãc)
                if ((c == 227 || c == 232 || c == 236 || c == 242 || c == 244 || c == 245 || c == 249) &&
                    i + 1 < n && "cptCPT".IndexOf(w[i + 1]) >= 0)
                {
                    return true;
                }
                // 2) é(233=ộ in TCVN3) followed by 'c' (e.g., thuéc, cuéc, méc, léc, đéc; excluding loanword 'séc'/'xéc')
                if (c == 233 && i + 1 < n && (w[i + 1] == 'c' || w[i + 1] == 'C') &&
                    (i == 0 || "sSxX".IndexOf(w[i - 1]) < 0))
                {
                    return true;
                }
                // 3) ó(243=ú in TCVN3) or ô(244=ụ in TCVN3) followed by 'y' (e.g., Thóy=Thúy, Qóy=Quý, Thôy=Thụy, ngôy=ngụy)
                if ((c == 243 || c == 244) && i + 1 < n && (w[i + 1] == 'y' || w[i + 1] == 'Y'))
                {
                    return true;
                }
            }
            return false;
        }

        static bool WordHasRealUnicode(string w)
        {
            for (int i = 0; i < w.Length; i++)
            {
                int c = NormByte(w[i]);
                if (c > 255) return true;
            }
            return false;
        }

        static string ConvertVniWord(string s)
        {
            StringBuilder sb = new StringBuilder(s.Length);
            int n = s.Length;
            int i = 0;
            while (i < n)
            {
                char ch1 = s[i];
                int c1 = NormByte(ch1);
                int c2 = (i + 1 < n) ? NormByte(s[i + 1]) : -1;

                int pairUni = (c2 != -1) ? TryGetVniPairUni(c1, c2) : 0;
                if (pairUni > 0)
                {
                    sb.Append((char)pairUni);
                    i += 2;
                    continue;
                }

                int singleUni;
                if (VniSingleMap.TryGetValue(c1, out singleUni))
                {
                    sb.Append((char)singleUni);
                }
                else if (c1 == 208) // Ð -> Đ
                {
                    sb.Append('\u0110');
                }
                else
                {
                    sb.Append(ch1);
                }
                i++;
            }
            return sb.ToString();
        }

        static string ConvertTcvnWord(string s)
        {
            StringBuilder sb = new StringBuilder(s.Length);
            int n = s.Length;
            for (int i = 0; i < n; i++)
            {
                char ch = s[i];
                int c = NormByte(ch);
                // Special case for byte 208 (Ð):
                // At the start of a word or in ALL-CAPS (e.g. ÐOÀN, ÐỖ, CMÐ, ÐKL), 208 is Đ (272), NOT TCVN3 é (233)!
                if (c == 208)
                {
                    bool isAllCapsOrStart = (i == 0) || (s[i - 1] >= 'A' && s[i - 1] <= 'Z');
                    if (isAllCapsOrStart)
                    {
                        sb.Append('\u0110');
                        continue;
                    }
                }
                int uni;
                if (TcvnMap.TryGetValue(c, out uni))
                    sb.Append((char)uni);
                else
                    sb.Append(ch);
            }
            return sb.ToString();
        }

        static bool IsWordChar(char c)
        {
            if (c <= ' ') return false;
            if ("/\\,;:|()[]{}<>\"'=+*?&~`!@#$%^.-".IndexOf(c) >= 0) return false;
            return true;
        }

        // Decodes a single line or segment by inspecting both line-level and word-level encodings
        static string DecodeSegmentSmart(string seg, string defaultStyleMode)
        {
            if (string.IsNullOrEmpty(seg)) return "";

            // First, check if any word in this segment has strong VNI, strong TCVN3, or pure Unicode
            bool segHasVni = false;
            bool segHasTcvn = false;
            bool segHasPureUni = false;
            int n = seg.Length;
            int idx = 0;
            while (idx < n)
            {
                if (!IsWordChar(seg[idx])) { idx++; continue; }
                int start = idx;
                while (idx < n && IsWordChar(seg[idx])) idx++;
                string w = seg.Substring(start, idx - start);
                bool sv = WordHasStrongVni(w);
                bool st = !sv && WordHasStrongTcvn(w);
                bool ru = WordHasRealUnicode(w);
                if (sv) segHasVni = true;
                else if (st) segHasTcvn = true;
                else if (ru) segHasPureUni = true;
            }

            // Now decode word by word!
            StringBuilder sb = new StringBuilder(seg.Length);
            idx = 0;
            while (idx < n)
            {
                if (!IsWordChar(seg[idx]))
                {
                    sb.Append(seg[idx]);
                    idx++;
                    continue;
                }
                int start = idx;
                while (idx < n && IsWordChar(seg[idx])) idx++;
                string w = seg.Substring(start, idx - start);

                bool wStrongVni = WordHasStrongVni(w);
                bool wStrongTcvn = !wStrongVni && WordHasStrongTcvn(w);
                bool wRealUni = WordHasRealUnicode(w);

                if (wStrongVni)
                {
                    sb.Append(ConvertVniWord(w));
                }
                else if (wStrongTcvn)
                {
                    sb.Append(ConvertTcvnWord(w));
                }
                else if (!wRealUni)
                {
                    // Word has only <= 255 bytes and no strong self-indicator (e.g. "THÒ", "HOÀNG", "Phó", "Bích", "sè", "tê")
                    if (segHasVni || (!segHasTcvn && !segHasPureUni && defaultStyleMode == "VNI"))
                    {
                        sb.Append(ConvertVniWord(w));
                    }
                    else if ((segHasTcvn && !segHasPureUni) || (!segHasPureUni && defaultStyleMode == "TCVN"))
                    {
                        sb.Append(ConvertTcvnWord(w));
                    }
                    else
                    {
                        // Check if word has Ð (208) at start or in caps (e.g. ÐOÀN, CMÐ)
                        if (w.IndexOf('\u00D0') >= 0)
                            w = w.Replace('\u00D0', '\u0110');
                        sb.Append(w);
                    }
                }
                else
                {
                    // Word has real Unicode (> 255), e.g. "Phạm", "THỬA", or "ÐỖ"
                    if (w.IndexOf('\u00D0') >= 0)
                        w = w.Replace('\u00D0', '\u0110');
                    sb.Append(w);
                }
            }
            return sb.ToString();
        }

        public static void DecodeCadText(string raw, string styleAndFont, out string displayUni, out string plainAscii)
        {
            if (string.IsNullOrEmpty(raw))
            {
                displayUni = "";
                plainAscii = "";
                return;
            }
            string s1 = ExpandHexAndPct(raw);
            string inlineMode;
            string s2 = CleanMTextAndDetectInlineFont(s1, out inlineMode);

            // Normalize Unicode combining diacritics (NFD -> NFC) so 'a' + \u0301 becomes 'á'
            try
            {
                s2 = s2.Normalize(NormalizationForm.FormC);
            }
            catch { }

            string stUp = (styleAndFont ?? "").ToUpperInvariant();
            string defaultStyleMode = inlineMode;
            if (defaultStyleMode == null)
            {
                if (stUp.Contains(".VN") || stUp.Contains("VNARIAL") || stUp.Contains("VHARIAL") || stUp.Contains("VNTIME") || stUp.Contains("VKNT") || stUp.Contains("TCVN") || stUp.Contains("ARIAL-REG") || stUp.Contains("ARIAL-BOL") || stUp.Contains("ARIAL-LIC") || stUp.Contains("HCM") || stUp.Contains("CMLIGHT"))
                    defaultStyleMode = "TCVN";
                else if (stUp.Contains("VNI") || stUp.Contains("VTIMES") || stUp.Contains("AVO") || stUp.Contains("VHELVE") || stUp.Contains("APTIMA") || stUp.Contains("CMTXT") || stUp.Contains("D100"))
                    defaultStyleMode = "VNI";
                else
                    defaultStyleMode = "UNI";
            }

            // Split by " / " (MText lines) so each line gets its own line-context while preserving word-level precision
            string[] segs = s2.Split(new string[] { " / " }, StringSplitOptions.None);
            if (segs.Length <= 1)
            {
                displayUni = DecodeSegmentSmart(s2, defaultStyleMode);
            }
            else
            {
                StringBuilder sb = new StringBuilder(s2.Length);
                for (int i = 0; i < segs.Length; i++)
                {
                    if (i > 0) sb.Append(" / ");
                    sb.Append(DecodeSegmentSmart(segs[i], defaultStyleMode));
                }
                displayUni = sb.ToString();
            }

            displayUni = CollapseSpaces(displayUni);
            plainAscii = ToPlainAscii(displayUni);
        }

        static string CollapseSpaces(string s)
        {
            if (string.IsNullOrEmpty(s)) return "";
            StringBuilder sb = new StringBuilder(s.Length);
            bool lastSpace = false;
            foreach (char c in s)
            {
                if (c == '\r' || c == '\n' || c == '\t' || c == ' ')
                {
                    if (!lastSpace)
                    {
                        sb.Append(' ');
                        lastSpace = true;
                    }
                }
                else
                {
                    sb.Append(c);
                    lastSpace = false;
                }
            }
            return sb.ToString().Trim();
        }

        public static string ToPlainAscii(string uni)
        {
            if (string.IsNullOrEmpty(uni)) return "";
            StringBuilder sb = new StringBuilder(uni.Length);
            foreach (char c in uni)
            {
                // Skip Unicode Combining Diacritical Marks (0x0300..0x036F) and soft hyphen (0x00AD)
                int code = (int)c;
                if (code >= 0x0300 && code <= 0x036F) continue;
                char a = CharToAsciiUpper(c);
                if (a != '\0')
                    sb.Append(a);
            }
            return sb.ToString();
        }

        static char CharToAsciiUpper(char c)
        {
            if (c >= 'a' && c <= 'z') return (char)(c - 32);
            if (c < 128) return c;
            switch ((int)c)
            {
                case 192: case 193: case 194: case 195: case 196: case 197:
                case 224: case 225: case 226: case 227: case 228: case 229:
                case 258: case 259:
                case 7840: case 7841: case 7842: case 7843: case 7844: case 7845: case 7846: case 7847: case 7848: case 7849:
                case 7850: case 7851: case 7852: case 7853: case 7854: case 7855: case 7856: case 7857: case 7858: case 7859:
                case 7860: case 7861: case 7862: case 7863:
                    return 'A';
                case 200: case 201: case 202: case 203:
                case 232: case 233: case 234: case 235:
                case 7864: case 7865: case 7866: case 7867: case 7868: case 7869:
                case 7870: case 7871: case 7872: case 7873: case 7874: case 7875: case 7876: case 7877: case 7878: case 7879:
                    return 'E';
                case 204: case 205: case 206: case 207:
                case 236: case 237: case 238: case 239:
                case 296: case 297:
                case 7880: case 7881: case 7882: case 7883:
                    return 'I';
                case 210: case 211: case 212: case 213: case 214:
                case 242: case 243: case 244: case 245: case 246:
                case 416: case 417:
                case 7884: case 7885: case 7886: case 7887: case 7888: case 7889: case 7890: case 7891: case 7892: case 7893:
                case 7894: case 7895: case 7896: case 7897: case 7898: case 7899: case 7900: case 7901: case 7902: case 7903:
                case 7904: case 7905: case 7906: case 7907:
                    return 'O';
                case 217: case 218: case 219: case 220:
                case 249: case 250: case 251: case 252:
                case 360: case 361: case 431: case 432:
                case 7908: case 7909: case 7910: case 7911: case 7912: case 7913: case 7914: case 7915: case 7916: case 7917:
                case 7918: case 7919: case 7920: case 7921:
                    return 'U';
                case 221: case 253: case 255: case 376:
                case 7922: case 7923: case 7924: case 7925: case 7926: case 7927: case 7928: case 7929:
                    return 'Y';
                case 272: case 273: case 208: case 209: case 241:
                    return 'D';
                case 178:
                    return '2'; // m² -> M2
                case 179:
                    return '3'; // m³ -> M3
                default:
                    return c;
            }
        }

        static string EncodeUPlusForLisp(string s)
        {
            if (string.IsNullOrEmpty(s)) return "";
            StringBuilder sb = new StringBuilder(s.Length * 2);
            foreach (char c in s)
            {
                if (c < 128)
                    sb.Append(c);
                else
                    sb.AppendFormat("\\U+{0:X4}", (int)c);
            }
            return sb.ToString();
        }

        static string WorkDir = GetDefaultWorkDir();
        static string GetDefaultWorkDir()
        {
            try
            {
                string exeDir = AppDomain.CurrentDomain.BaseDirectory.TrimEnd('\\', '/');
                if (File.Exists(Path.Combine(exeDir, "FOLDER_INDEX_DB.tsv")) ||
                    exeDir.EndsWith("EXTRACUUAUTOCAD", StringComparison.OrdinalIgnoreCase))
                    return exeDir;
            }
            catch { }
            if (Directory.Exists(@"D:\EXTRACUUAUTOCAD") || Directory.Exists(@"D:\"))
                return @"D:\EXTRACUUAUTOCAD";
            return @"C:\EXTRACUUAUTOCAD";
        }

        static string LoadLastFolder()
        {
            try
            {
                string[] candidates = new string[] {
                    Path.Combine(WorkDir, "last_folder.cfg"),
                    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "last_folder.cfg"),
                    @"D:\EXTRACUUAUTOCAD\last_folder.cfg",
                    @"C:\EXTRACUUAUTOCAD\last_folder.cfg"
                };
                foreach (string file in candidates)
                {
                    if (File.Exists(file))
                    {
                        string content = File.ReadAllText(file, Encoding.UTF8).Trim();
                        if (!string.IsNullOrEmpty(content))
                            return content;
                    }
                }
            }
            catch { }
            return "";
        }

        static void SaveLastFolder(string folder)
        {
            if (string.IsNullOrEmpty(folder)) return;
            try
            {
                string[] targets = new string[] {
                    Path.Combine(WorkDir, "last_folder.cfg"),
                    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "last_folder.cfg"),
                    @"D:\EXTRACUUAUTOCAD\last_folder.cfg",
                    @"C:\EXTRACUUAUTOCAD\last_folder.cfg"
                };
                foreach (string target in targets)
                {
                    try
                    {
                        string dir = Path.GetDirectoryName(target);
                        if (!string.IsNullOrEmpty(dir) && Directory.Exists(dir))
                        {
                            File.WriteAllText(target, folder, Encoding.UTF8);
                        }
                    }
                    catch { }
                }
            }
            catch { }
        }
        static string ManifestPath
        {
            get
            {
                string p = Path.Combine(WorkDir, "FOLDER_MANIFEST.tsv");
                if (File.Exists(p)) return p;
                string[] cands = new string[] {
                    @"D:\EXTRACUUAUTOCAD\FOLDER_MANIFEST.tsv",
                    @"C:\EXTRACUUAUTOCAD\FOLDER_MANIFEST.tsv",
                    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "FOLDER_MANIFEST.tsv"),
                    @"D:\New folder\DAT\BO_FONT_CAD_FULL\FOLDER_MANIFEST.tsv",
                    @"D:\New folder\DAT\TANGNHONPHUHTR\FOLDER_MANIFEST.tsv",
                    @"D:\VBA\FOLDER_MANIFEST.tsv",
                    @"C:\VBA\FOLDER_MANIFEST.tsv"
                };
                foreach (string c in cands) { if (File.Exists(c)) return c; }
                return p;
            }
        }

        static string IndexDbPath
        {
            get
            {
                string p = Path.Combine(WorkDir, "FOLDER_INDEX_DB.tsv");
                if (!File.Exists(p) && File.Exists(p + ".tmp"))
                {
                    try { File.Move(p + ".tmp", p); } catch { }
                }
                if (File.Exists(p)) return p;
                string[] cands = new string[] {
                    @"D:\EXTRACUUAUTOCAD\FOLDER_INDEX_DB.tsv",
                    @"C:\EXTRACUUAUTOCAD\FOLDER_INDEX_DB.tsv",
                    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "FOLDER_INDEX_DB.tsv"),
                    @"D:\New folder\DAT\BO_FONT_CAD_FULL\FOLDER_INDEX_DB.tsv",
                    @"D:\VBA\FOLDER_INDEX_DB.tsv",
                    @"C:\VBA\FOLDER_INDEX_DB.tsv"
                };
                foreach (string c in cands)
                {
                    if (!File.Exists(c) && File.Exists(c + ".tmp"))
                    {
                        try { File.Move(c + ".tmp", c); } catch { }
                    }
                    if (File.Exists(c)) return c;
                }
                return p;
            }
        }

        static string MapPath
        {
            get
            {
                string db = IndexDbPath;
                return Path.ChangeExtension(db, ".dwgmap");
            }
        }

        public class DwgBlock
        {
            public string DwgPath;
            public string DwgNameUpper;
            public string ParentDirUpper;
            public string GrandParentUpper;
            public string FullPrefix;
            public long Offset;
            public int Length;
            public bool IsTongGhep;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static unsafe bool ContainsBytes(byte* line, int len, byte[] pat)
        {
            int pLen = pat.Length;
            if (len < pLen) return false;
            byte p0 = pat[0];
            int max = len - pLen;
            for (int i = 0; i <= max; i++)
            {
                if (line[i] == p0)
                {
                    int j = 1;
                    while (j < pLen && line[i + j] == pat[j]) j++;
                    if (j == pLen) return true;
                }
            }
            return false;
        }

        static unsafe List<DwgBlock> GetOrBuildDwgMap()
        {
            if (!File.Exists(IndexDbPath)) return new List<DwgBlock>();
            var fi = new FileInfo(IndexDbPath);
            long dbLen = fi.Length;
            long dbTicks = fi.LastWriteTimeUtc.Ticks;

            if (File.Exists(MapPath))
            {
                try
                {
                    using (var sr = new StreamReader(MapPath, Utf8NoBom))
                    {
                        string header = sr.ReadLine();
                        if (header != null)
                        {
                            string[] hp = header.Split('\t');
                            if (hp.Length >= 2 && long.Parse(hp[0]) == dbLen && long.Parse(hp[1]) == dbTicks)
                            {
                                var list = new List<DwgBlock>(2048);
                                string line;
                                while ((line = sr.ReadLine()) != null)
                                {
                                    string[] p = line.Split('\t');
                                    if (p.Length >= 4)
                                    {
                                        string dwg = p[0];
                                        long off = long.Parse(p[1]);
                                        int len = int.Parse(p[2]);
                                        bool isTg = p[3] == "1";
                                        string nameUp = Path.GetFileNameWithoutExtension(dwg).ToUpperInvariant();
                                        string dirUp = Path.GetFileName(Path.GetDirectoryName(dwg) ?? "").ToUpperInvariant();
                                        string grandUp = Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(dwg) ?? "") ?? "").ToUpperInvariant();
                                        list.Add(new DwgBlock
                                        {
                                            DwgPath = dwg,
                                            DwgNameUpper = nameUp,
                                            ParentDirUpper = dirUp,
                                            GrandParentUpper = grandUp,
                                            FullPrefix = nameUp + " " + dirUp + " " + grandUp + " ",
                                            Offset = off,
                                            Length = len,
                                            IsTongGhep = isTg
                                        });
                                    }
                                }
                                return list;
                            }
                        }
                    }
                }
                catch { }
            }

            var builtList = new List<DwgBlock>(2048);
            using (var mmf = MemoryMappedFile.CreateFromFile(IndexDbPath, FileMode.Open, null, 0, MemoryMappedFileAccess.Read))
            using (var va = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read))
            {
                byte* basePtr = null;
                va.SafeMemoryMappedViewHandle.AcquirePointer(ref basePtr);
                try
                {
                    long len = fi.Length;
                    long pos = 0;
                    string curDwg = null;
                    long blockStart = 0;

                    while (pos < len)
                    {
                        long lineStart = pos;
                        while (pos < len && basePtr[pos] != (byte)'\t' && basePtr[pos] != (byte)'\n') pos++;
                        if (pos < len && basePtr[pos] == (byte)'\t')
                        {
                            int dwgLen = (int)(pos - lineStart);
                            string dwg = Encoding.UTF8.GetString(basePtr + lineStart, dwgLen);
                            if (dwg != curDwg)
                            {
                                if (curDwg != null)
                                {
                                    AddDwgBlock(builtList, curDwg, blockStart, (int)(lineStart - blockStart));
                                }
                                curDwg = dwg;
                                blockStart = lineStart;
                            }
                        }
                        while (pos < len && basePtr[pos] != (byte)'\n') pos++;
                        if (pos < len && basePtr[pos] == (byte)'\n') pos++;
                    }
                    if (curDwg != null && blockStart < len)
                    {
                        AddDwgBlock(builtList, curDwg, blockStart, (int)(len - blockStart));
                    }
                }
                finally
                {
                    va.SafeMemoryMappedViewHandle.ReleasePointer();
                }
            }

            try
            {
                using (var sw = new StreamWriter(MapPath, false, Utf8NoBom))
                {
                    sw.WriteLine(dbLen + "\t" + dbTicks);
                    foreach (var b in builtList)
                    {
                        sw.WriteLine(b.DwgPath + "\t" + b.Offset + "\t" + b.Length + "\t" + (b.IsTongGhep ? "1" : "0"));
                    }
                }
            }
            catch { }

            return builtList;
        }

        static void AddDwgBlock(List<DwgBlock> list, string dwg, long offset, int length)
        {
            string nameUp = Path.GetFileNameWithoutExtension(dwg).ToUpperInvariant();
            string dirUp = Path.GetFileName(Path.GetDirectoryName(dwg) ?? "").ToUpperInvariant();
            string grandUp = Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(dwg) ?? "") ?? "").ToUpperInvariant();
            bool isTg = nameUp.Contains("TONG GHEP") || nameUp.Contains("TONGGHEP") ||
                        nameUp.Contains("TONG_GHEP") || nameUp.Contains("TONG-GHEP") ||
                        nameUp.Contains("TONG HOP") || nameUp.Contains("TONGHOP");

            list.Add(new DwgBlock
            {
                DwgPath = dwg,
                DwgNameUpper = nameUp,
                ParentDirUpper = dirUp,
                GrandParentUpper = grandUp,
                FullPrefix = nameUp + " " + dirUp + " " + grandUp + " ",
                Offset = offset,
                Length = length,
                IsTongGhep = isTg
            });
        }

        static Dictionary<string, string> PathResolutionCache = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        static string ResolveExistingDwgPath(string dwgPath)
        {
            if (string.IsNullOrEmpty(dwgPath)) return "";
            string resolved;
            lock (PathResolutionCache)
            {
                if (PathResolutionCache.TryGetValue(dwgPath, out resolved))
                    return resolved;
            }

            if (File.Exists(dwgPath))
            {
                lock (PathResolutionCache) { PathResolutionCache[dwgPath] = dwgPath; }
                return dwgPath;
            }

            string fnOnly = Path.GetFileName(dwgPath);

            // 1. Kiem tra truc tiep trong thu muc dang tim kiem tren may nay
            string actFld = FastFindForm.ActiveSearchFolder;
            if (!string.IsNullOrEmpty(actFld) && !actFld.Equals("ALL", StringComparison.OrdinalIgnoreCase) && Directory.Exists(actFld))
            {
                if (!string.IsNullOrEmpty(fnOnly))
                {
                    string localCand = Path.Combine(actFld, fnOnly);
                    if (File.Exists(localCand))
                    {
                        lock (PathResolutionCache) { PathResolutionCache[dwgPath] = localCand; }
                        return localCand;
                    }
                }
            }

            // 2. Chuyen doi linh hoat ky tu o dia (D: -> C:, E:, F:...) khi cai o may khac
            if (dwgPath.Length >= 3 && char.IsLetter(dwgPath[0]) && dwgPath[1] == ':')
            {
                string subPath = dwgPath.Substring(2);
                foreach (var d in Environment.GetLogicalDrives())
                {
                    string altDrive = d.Substring(0, 2) + subPath;
                    if (File.Exists(altDrive))
                    {
                        lock (PathResolutionCache) { PathResolutionCache[dwgPath] = altDrive; }
                        return altDrive;
                    }
                }
            }

            string fn = Path.GetFileName(dwgPath);
            if (!string.IsNullOrEmpty(fn))
            {
                string cand1 = Path.Combine(@"D:\New folder\DAT\TANGNHONPHUHTR\TNP\BD_GOC\2. Hiepphu\TL200", fn);
                if (File.Exists(cand1))
                {
                    lock (PathResolutionCache) { PathResolutionCache[dwgPath] = cand1; }
                    return cand1;
                }
                string cand2 = Path.Combine(@"D:\DAT\9999-2\9999-3\ANKHANH-8-9", fn);
                if (File.Exists(cand2))
                {
                    lock (PathResolutionCache) { PathResolutionCache[dwgPath] = cand2; }
                    return cand2;
                }
            }

            lock (PathResolutionCache) { PathResolutionCache[dwgPath] = dwgPath; }
            return dwgPath;
        }

        static Dictionary<string, string> LoadManifest()
        {
            var dict = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            if (File.Exists(ManifestPath))
            {
                foreach (string line in File.ReadAllLines(ManifestPath, Utf8NoBom))
                {
                    if (string.IsNullOrEmpty(line)) continue;
                    int tab = line.IndexOf('\t');
                    if (tab > 0)
                    {
                        dict[line.Substring(0, tab)] = line.Substring(tab + 1);
                    }
                }
            }
            return dict;
        }

        static void SaveManifest(Dictionary<string, string> dict)
        {
            Directory.CreateDirectory(WorkDir);
            using (var sw = new StreamWriter(ManifestPath, false, Utf8NoBom))
            {
                foreach (var kv in dict)
                {
                    sw.Write(kv.Key);
                    sw.Write('\t');
                    sw.WriteLine(kv.Value);
                }
            }
        }

        static string GetFileSig(FileInfo fi)
        {
            return fi.LastWriteTimeUtc.Ticks.ToString() + "|" + fi.Length.ToString();
        }

        static string ExtractSigSize(string sig)
        {
            if (string.IsNullOrEmpty(sig)) return "";
            int bar = sig.IndexOf('|');
            return (bar >= 0 && bar + 1 < sig.Length) ? sig.Substring(bar + 1) : sig;
        }

        static bool ShouldSkipDir(string dirName)
        {
            string up = dirName.ToUpperInvariant();
            return up == "$RECYCLE.BIN" || up == "SYSTEM VOLUME INFORMATION" || up == "EXTRACUUAUTOCAD" || up == "BO_FONT_CAD_FULL" || up == "ZALO DATA";
        }

        static void CollectDwgs(string folder, List<FileInfo> list)
        {
            try
            {
                var di = new DirectoryInfo(folder);
                if (!di.Exists) return;
                foreach (var fi in di.GetFiles("*.dwg"))
                {
                    if (!fi.Name.StartsWith("~$"))
                        list.Add(fi);
                }
                foreach (var sub in di.GetDirectories())
                {
                    if (!ShouldSkipDir(sub.Name))
                        CollectDwgs(sub.FullName, list);
                }
            }
            catch { }
        }

        static void DoCheck(string folderPath, string todoFilePath, bool force)
        {
            Directory.CreateDirectory(WorkDir);
            var manifest = force ? new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase) : LoadManifest();
            var allDwgs = new List<FileInfo>();
            CollectDwgs(folderPath, allDwgs);

            // Build lookup by (FileName|FileSize) -> existing indexed path so moved/copied folders on another PC never need re-indexing!
            var byNameAndSize = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            if (!force)
            {
                foreach (var kv in manifest)
                {
                    string sz = ExtractSigSize(kv.Value);
                    if (!string.IsNullOrEmpty(sz))
                    {
                        string key = Path.GetFileName(kv.Key) + "|" + sz;
                        if (!byNameAndSize.ContainsKey(key))
                            byNameAndSize[key] = kv.Key;
                    }
                }
            }

            var todo = new List<string>();
            var remapFromTo = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
            bool manifestUpdated = false;

            foreach (var fi in allDwgs)
            {
                string sig = GetFileSig(fi);
                string sizeStr = fi.Length.ToString();
                string oldSig;
                if (!force && manifest.TryGetValue(fi.FullName, out oldSig))
                {
                    if (oldSig == sig)
                        continue;
                    if (ExtractSigSize(oldSig) == sizeStr)
                    {
                        // File size is identical (only timestamp drifted from copying via USB/Zalo)
                        manifest[fi.FullName] = sig;
                        manifestUpdated = true;
                        continue;
                    }
                }
                else if (!force)
                {
                    string srcPath;
                    string lookupKey = fi.Name + "|" + sizeStr;
                    if (byNameAndSize.TryGetValue(lookupKey, out srcPath) && File.Exists(IndexDbPath))
                    {
                        List<string> targets;
                        if (!remapFromTo.TryGetValue(srcPath, out targets))
                        {
                            targets = new List<string>();
                            remapFromTo[srcPath] = targets;
                        }
                        targets.Add(fi.FullName);
                        manifest[fi.FullName] = sig;
                        manifestUpdated = true;
                        continue;
                    }
                }

                todo.Add(EncodeUPlusForLisp(fi.FullName));
            }

            if (remapFromTo.Count > 0 && File.Exists(IndexDbPath))
            {
                var clonedLines = new List<string>(16384);
                using (var sr = new StreamReader(IndexDbPath, Utf8NoBom, false, 65536))
                {
                    string line;
                    while ((line = sr.ReadLine()) != null)
                    {
                        int tab = line.IndexOf('\t');
                        if (tab > 0)
                        {
                            string fp = line.Substring(0, tab);
                            List<string> targets;
                            if (remapFromTo.TryGetValue(fp, out targets))
                            {
                                string rest = line.Substring(tab);
                                for (int t = 0; t < targets.Count; t++)
                                    clonedLines.Add(targets[t] + rest);
                            }
                        }
                    }
                }
                if (clonedLines.Count > 0)
                {
                    using (var sw = new StreamWriter(IndexDbPath, true, Utf8NoBom, 65536))
                    {
                        foreach (string cl in clonedLines)
                            sw.WriteLine(cl);
                    }
                }
            }

            if (manifestUpdated)
                SaveManifest(manifest);

            File.WriteAllLines(todoFilePath, todo.ToArray(), Encoding.ASCII);
            File.WriteAllText(todoFilePath + ".stat", allDwgs.Count + "|" + todo.Count, Encoding.ASCII);
        }

        static void DoIngest(string rawTsvPath)
        {
            if (!File.Exists(rawTsvPath)) return;
            Directory.CreateDirectory(WorkDir);

            var manifest = LoadManifest();
            var updatedFiles = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var newIndexLines = new List<string>(32768);

            using (var fs = new FileStream(rawTsvPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
            using (var sr = new StreamReader(fs, Utf8NoBom))
            {
                string line;
                while ((line = sr.ReadLine()) != null)
                {
                    if (string.IsNullOrEmpty(line)) continue;
                    if (line.StartsWith("#FILE#\t"))
                    {
                        string fpath = line.Substring(7).Trim();
                        if (fpath.Length > 0)
                            updatedFiles.Add(fpath);
                        continue;
                    }
                    string[] parts = line.Split('\t');
                    if (parts.Length < 7) continue;

                    string dwgPath = parts[0];
                    string layer = parts[1];
                    string entType = parts[2];
                    string styleFont = parts[3];
                    string xStr = parts[4];
                    string yStr = parts[5];
                    string rawText = parts[6];
                    if (parts.Length > 7)
                    {
                        StringBuilder rsb = new StringBuilder(rawText);
                        for (int k = 7; k < parts.Length; k++)
                            rsb.Append(' ').Append(parts[k]);
                        rawText = rsb.ToString();
                    }

                    updatedFiles.Add(dwgPath);

                    string dispUni, plainAsc;
                    DecodeCadText(rawText, styleFont, out dispUni, out plainAsc);
                    if (string.IsNullOrEmpty(dispUni)) continue;

                    newIndexLines.Add(string.Join("\t", new string[] {
                        dwgPath, layer, entType, xStr, yStr, dispUni, plainAsc
                    }));
                }
            }

            if (updatedFiles.Count == 0) return;

            foreach (string fp in updatedFiles)
            {
                try
                {
                    var fi = new FileInfo(fp);
                    if (fi.Exists)
                        manifest[fp] = GetFileSig(fi);
                }
                catch { }
            }

            string targetDb = IndexDbPath;
            string tempDb = targetDb + ".tmp";
            using (var sw = new StreamWriter(tempDb, false, Utf8NoBom, 65536))
            {
                if (File.Exists(targetDb))
                {
                    using (var sr = new StreamReader(targetDb, Utf8NoBom, false, 65536))
                    {
                        string line;
                        while ((line = sr.ReadLine()) != null)
                        {
                            int tab = line.IndexOf('\t');
                            if (tab > 0)
                            {
                                string fp = line.Substring(0, tab);
                                if (updatedFiles.Contains(fp)) continue;
                                sw.WriteLine(line);
                            }
                        }
                    }
                }
                foreach (string nl in newIndexLines)
                {
                    sw.WriteLine(nl);
                }
            }
            if (File.Exists(targetDb))
            {
                try { File.Delete(targetDb); } catch { }
            }
            File.Move(tempDb, targetDb);
            try { if (File.Exists(MapPath)) File.Delete(MapPath); } catch { }
            SaveManifest(manifest);
        }

        // Re-decodes all existing lines in FOLDER_INDEX_DB.tsv using the upgraded word-by-word multi-font engine
        static void DoRedecode()
        {
            if (!File.Exists(IndexDbPath)) return;
            string targetDb = IndexDbPath;
            string tempDb = targetDb + ".tmp";
            int fixedCount = 0;
            using (var sw = new StreamWriter(tempDb, false, Utf8NoBom, 65536))
            using (var sr = new StreamReader(targetDb, Utf8NoBom, false, 65536))
            {
                string line;
                while ((line = sr.ReadLine()) != null)
                {
                    string[] parts = line.Split('\t');
                    if (parts.Length < 7) continue;
                    string oldUni = parts[5];
                    string oldAsc = parts[6];
                    string newUni, newAsc;
                    DecodeCadText(oldUni, "", out newUni, out newAsc);
                    if (newUni != oldUni || newAsc != oldAsc)
                        fixedCount++;
                    sw.WriteLine(string.Join("\t", new string[] {
                        parts[0], parts[1], parts[2], parts[3], parts[4], newUni, newAsc
                    }));
                }
            }
            if (File.Exists(targetDb))
            {
                try { File.Delete(targetDb); } catch { }
            }
            File.Move(tempDb, targetDb);
            Console.WriteLine("Redecoded DB lines fixed: " + fixedCount);
        }

        static bool WildcardMatch(string text, string pattern)
        {
            int ti = 0, pi = 0, starIdx = -1, matchIdx = 0;
            while (ti < text.Length)
            {
                if (pi < pattern.Length && (pattern[pi] == '?' || pattern[pi] == text[ti]))
                {
                    ti++;
                    pi++;
                }
                else if (pi < pattern.Length && pattern[pi] == '*')
                {
                    starIdx = pi;
                    matchIdx = ti;
                    pi++;
                }
                else if (starIdx != -1)
                {
                    pi = starIdx + 1;
                    matchIdx++;
                    ti = matchIdx;
                }
                else
                {
                    return false;
                }
            }
            while (pi < pattern.Length && pattern[pi] == '*') pi++;
            return pi == pattern.Length;
        }

        static HashSet<string> CadastralStopwords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "THUA", "TO", "SO", "GCN", "DAT", "BAN", "DO", "BD", "SDN", "TL",
            "CHU", "SU", "DUNG", "HO", "ONG", "BA", "VA", "CUNG", "CHONG", "VO",
            "DUONG", "HEM", "KP", "KHU", "PHO", "AP", "PHUONG", "XA", "QUAN",
            "DT", "DIEN", "TICH", "M2", "M"
        };

        static string StripCadastralPrefix(string qAsc)
        {
            string[] prefixes = new string[] {
                "ONG VA BA ", "BA VA ONG ", "CHU SU DUNG ", "CHU HO ", "ONG ", "BA ",
                "THUA DAT SO ", "THUA DAT ", "SO THUA ", "THUA SO ", "THUA ",
                "TO BAN DO SO ", "TO BAN DO ", "TO BD ", "SO TO ", "TO SO ", "TO ",
                "GIAY CHUNG NHAN SO ", "GIAY CHUNG NHAN ", "SO GCN ", "GCN SO ", "GCN ",
                "DUONG SO ", "DUONG ", "HEM SO ", "HEM "
            };
            foreach (string p in prefixes)
            {
                if (qAsc.StartsWith(p, StringComparison.Ordinal) && qAsc.Length > p.Length)
                {
                    return qAsc.Substring(p.Length).Trim();
                }
            }
            return qAsc;
        }

        static List<string> ExtractSearchTokens(string s)
        {
            var list = new List<string>();
            char[] seps = new char[] { ' ', '/', '-', '_', ',', ';', '|', '\\', '\t', '(', ')', '[', ']', ':', '.' };
            string[] raw = s.Split(seps, StringSplitOptions.RemoveEmptyEntries);
            foreach (string t in raw)
            {
                string c = t.Trim().ToUpperInvariant();
                if (c.Length > 0 && !CadastralStopwords.Contains(c))
                    list.Add(c);
            }
            return list;
        }

        static bool ContainsNumberToken(string text, string token)
        {
            if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(token)) return false;
            int idx = 0;
            while ((idx = text.IndexOf(token, idx, StringComparison.OrdinalIgnoreCase)) >= 0)
            {
                bool startOk = (idx == 0) || !char.IsDigit(text[idx - 1]);
                bool endOk = (idx + token.Length == text.Length) || !char.IsDigit(text[idx + token.Length]);
                if (startOk && endOk) return true;
                idx += token.Length;
            }
            return false;
        }

        static bool IsAllDigits(string token)
        {
            if (string.IsNullOrEmpty(token)) return false;
            for (int i = 0; i < token.Length; i++)
            {
                if (!char.IsDigit(token[i])) return false;
            }
            return true;
        }

        static bool ContainsTokenMatch(string text, string token)
        {
            if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(token)) return false;
            if (IsAllDigits(token))
                return ContainsNumberToken(text, token);
            return text.IndexOf(token, StringComparison.OrdinalIgnoreCase) >= 0;
        }


        // Builds smart abbreviation alternatives (e.g., "NGUYEN DUY TRINH" -> "NDTRINH", "ND TRINH")
        static List<string> BuildAbbrevVariants(string coreAsc)
        {
            var list = new List<string>();
            string[] words = coreAsc.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (words.Length >= 3)
            {
                // Initials of first N-1 words + last word: e.g. NGUYEN DUY TRINH -> NDTRINH, ND TRINH, N.D.TRINH
                StringBuilder initials = new StringBuilder();
                StringBuilder dotInitials = new StringBuilder();
                for (int i = 0; i < words.Length - 1; i++)
                {
                    initials.Append(words[i][0]);
                    dotInitials.Append(words[i][0]).Append('.');
                }
                string lastWord = words[words.Length - 1];
                if (lastWord.Length >= 3)
                {
                    list.Add(initials.ToString() + lastWord);
                    list.Add(initials.ToString() + " " + lastWord);
                    list.Add(dotInitials.ToString() + lastWord);
                }
            }
            if (words.Length >= 2 && words[0] == "NGUYEN")
            {
                // NGUYEN THI A -> NG THI A, NG T A
                string rest = string.Join(" ", words, 1, words.Length - 1);
                list.Add("NG " + rest);
                if (words.Length >= 3 && words[1] == "THI")
                {
                    string rest2 = string.Join(" ", words, 2, words.Length - 2);
                    list.Add("NG T " + rest2);
                    list.Add("NGUYEN T " + rest2);
                }
                else if (words.Length >= 3 && words[1] == "VAN")
                {
                    string rest2 = string.Join(" ", words, 2, words.Length - 2);
                    list.Add("NG V " + rest2);
                    list.Add("NGUYEN V " + rest2);
                }
            }
            return list;
        }

        // =========================================================================
        // FAST FIND PRO v13.0 - UPGRADED SMART SEARCH ENGINE & MODERN FORM UI
        // =========================================================================

        public class CadastralRecord
        {
            public int Stt;
            public string DwgPath;
            public string DwgName;
            public string ToBanDo;
            public string ParentDir;
            public string Layer;
            public string EntType;
            public string XStr;
            public string YStr;
            public string RawContent;
            public string NormalizedContent;
            public string ThuaLoai;
            public string ChuSuDungDiaChi;
            public string SoGcnNgayKy;
            public string CoordCad { get { return YStr + "," + XStr; } }
        }

        public static string NormalizeText(string input)
        {
            if (string.IsNullOrEmpty(input)) return "";
            string s = input.Trim().ToUpperInvariant();
            StringBuilder sb = new StringBuilder(s.Length);
            for (int i = 0; i < s.Length; i++)
            {
                char c = s[i];
                if (c == '【' || c == '】' || c == '[' || c == ']' || c == '(' || c == ')' || c == '"' || c == '\'')
                {
                    continue;
                }
                else if (c == '|' || c == '\t' || c == '\r' || c == '\n')
                {
                    sb.Append(' ');
                }
                else
                {
                    sb.Append(c);
                }
            }
            string res = sb.ToString();
            while (res.IndexOf("  ", StringComparison.Ordinal) >= 0)
            {
                res = res.Replace("  ", " ");
            }
            return res.Trim();
        }



        public static bool HasNumber(string s)
        {
            if (string.IsNullOrEmpty(s)) return false;
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] >= '0' && s[i] <= '9') return true;
            }
            return false;
        }

        public static bool HasWordBoundaryMatch(string text, string token)
        {
            if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(token)) return false;
            int idx = 0;
            int tLen = token.Length;
            int sLen = text.Length;

            while (idx <= sLen - tLen)
            {
                int found = text.IndexOf(token, idx, StringComparison.Ordinal);
                if (found < 0) return false;

                bool beforeOk = (found == 0) || !IsWordChar(text[found - 1]);
                bool afterOk = (found + tLen >= sLen) || !IsWordChar(text[found + tLen]);

                if (beforeOk && afterOk)
                    return true;

                idx = found + 1;
            }
            return false;
        }

        public static CadastralRecord ParseCadastralRecord(string dwgPath, string layer, string entType, string xStr, string yStr, string rawContent)
        {
            var rec = new CadastralRecord();
            rec.DwgPath = dwgPath ?? "";
            rec.DwgName = Path.GetFileName(rec.DwgPath);
            rec.ToBanDo = Path.GetFileNameWithoutExtension(rec.DwgPath);
            rec.ParentDir = Path.GetFileName(Path.GetDirectoryName(rec.DwgPath) ?? "");
            rec.Layer = layer ?? "";
            rec.EntType = entType ?? "";
            rec.XStr = xStr ?? "0.0";
            rec.YStr = yStr ?? "0.0";
            rec.RawContent = rawContent ?? "";
            rec.NormalizedContent = NormalizeText(rawContent);

            string text = rec.RawContent;
            bool hasSlash = Regex.IsMatch(text, @"\s+[\/\|]\s+") || (text.Contains("/") && text.Length > 20);

            if (hasSlash)
            {
                string[] rawParts = Regex.Split(text, @"\s*[\/\|]\s*");
                List<string> parts = new List<string>();
                foreach (var p in rawParts)
                {
                    string tp = p.Trim();
                    if (!string.IsNullOrEmpty(tp)) parts.Add(tp);
                }

                if (parts.Count >= 2)
                {
                    rec.ThuaLoai = parts[0] + " / " + parts[1];
                }
                else if (parts.Count == 1)
                {
                    rec.ThuaLoai = parts[0];
                }

                if (parts.Count >= 4)
                {
                    rec.ChuSuDungDiaChi = parts[2] + " - " + parts[3];
                }
                else if (parts.Count >= 3)
                {
                    rec.ChuSuDungDiaChi = parts[2];
                }

                for (int i = 2; i < parts.Count; i++)
                {
                    string p = parts[i];
                    if (p.Contains("【") || p.Contains("[") || Regex.IsMatch(p, @"\b[A-Za-z]{2}\b.*\d{4,}", RegexOptions.IgnoreCase) || p.ToUpperInvariant().Contains("GCN"))
                    {
                        rec.SoGcnNgayKy = p;
                        break;
                    }
                }
                if (string.IsNullOrEmpty(rec.SoGcnNgayKy) && parts.Count >= 6)
                {
                    rec.SoGcnNgayKy = parts[5];
                }
            }
            else
            {
                string upLay = rec.Layer.ToUpperInvariant();
                if (upLay.Contains("GCN") || text.Contains("【") || text.Contains("[") || Regex.IsMatch(text, @"\b[A-Za-z]{2}\b.*\d{4,}", RegexOptions.IgnoreCase))
                {
                    rec.SoGcnNgayKy = text;
                }
                else if (upLay.Contains("THUA") || upLay.Contains("LOAI"))
                {
                    rec.ThuaLoai = text;
                }
                else if (upLay.Contains("CSD") || upLay.Contains("TEN") || upLay.Contains("DIA CHI") || upLay.Contains("DC"))
                {
                    rec.ChuSuDungDiaChi = text;
                }
                else
                {
                    rec.SoGcnNgayKy = text;
                }
            }

            if (string.IsNullOrEmpty(rec.ThuaLoai)) rec.ThuaLoai = "---";
            if (string.IsNullOrEmpty(rec.ChuSuDungDiaChi)) rec.ChuSuDungDiaChi = "---";
            if (string.IsNullOrEmpty(rec.SoGcnNgayKy)) rec.SoGcnNgayKy = "---";

            return rec;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static unsafe bool LineContainsToken(byte* line, int len, byte[] tok)
        {
            int tokLen = tok.Length;
            if (len < tokLen) return false;

            byte firstLower = tok[0];
            byte firstUpper = tok[0];
            if (firstLower >= 97 && firstLower <= 122) firstUpper = (byte)(firstLower - 32);
            else if (firstLower >= 65 && firstLower <= 90) firstLower = (byte)(firstLower + 32);

            int maxStart = len - tokLen;
            for (int i = 0; i <= maxStart; i++)
            {
                byte b = line[i];
                if (b == firstLower || b == firstUpper)
                {
                    bool match = true;
                    for (int k = 1; k < tokLen; k++)
                    {
                        byte bk = line[i + k];
                        byte tk = tok[k];
                        if (bk != tk)
                        {
                            if (tk >= 97 && tk <= 122)
                            {
                                if (bk != (byte)(tk - 32)) { match = false; break; }
                            }
                            else if (tk >= 65 && tk <= 90)
                            {
                                if (bk != (byte)(tk + 32)) { match = false; break; }
                            }
                            else
                            {
                                match = false;
                                break;
                            }
                        }
                    }
                    if (match) return true;
                }
            }
            return false;
        }

        static Dictionary<string, HashSet<string>> FolderDwgCache = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);

        static HashSet<string> GetDwgsInFolder(string targetFolder)
        {
            if (string.IsNullOrEmpty(targetFolder) || targetFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase))
                return null;
            lock (FolderDwgCache)
            {
                HashSet<string> dwgSet;
                if (FolderDwgCache.TryGetValue(targetFolder, out dwgSet))
                    return dwgSet;
                dwgSet = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                try
                {
                    if (Directory.Exists(targetFolder))
                    {
                        var allFiles = Directory.GetFiles(targetFolder, "*.dwg", SearchOption.AllDirectories);
                        foreach (var f in allFiles)
                        {
                            string name = Path.GetFileName(f);
                            if (!string.IsNullOrEmpty(name))
                                dwgSet.Add(name);
                        }
                    }
                }
                catch { }
                FolderDwgCache[targetFolder] = dwgSet;
                return dwgSet;
            }
        }

        static bool PathMatchesFolder(string dwgPath, string targetFolder, HashSet<string> folderDwgNames)
        {
            if (string.IsNullOrEmpty(targetFolder) || targetFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase)) return true;
            string normTarget = targetFolder.Trim().TrimEnd('\\', '/');

            // 1. Neu duong dan trong CSDL bat dau bang thu muc nguoi dung chon
            if (dwgPath.StartsWith(normTarget + "\\", StringComparison.OrdinalIgnoreCase)) return true;
            if (string.Equals(Path.GetDirectoryName(dwgPath), normTarget, StringComparison.OrdinalIgnoreCase)) return true;

            // 2. So sanh bo qua ky tu o dia (C: vs D: vs E:)
            if (dwgPath.Length >= 3 && normTarget.Length >= 3 && dwgPath[1] == ':' && normTarget[1] == ':')
            {
                string dwgNoDrive = dwgPath.Substring(2);
                string targetNoDrive = normTarget.Substring(2);
                if (dwgNoDrive.StartsWith(targetNoDrive + "\\", StringComparison.OrdinalIgnoreCase)) return true;
                if (string.Equals(Path.GetDirectoryName(dwgNoDrive), targetNoDrive, StringComparison.OrdinalIgnoreCase)) return true;
            }

            // 3. So sanh ten thu muc con cuoi cung
            string targetFolderName = Path.GetFileName(normTarget);
            if (!string.IsNullOrEmpty(targetFolderName))
            {
                string dwgDir = Path.GetDirectoryName(dwgPath);
                if (!string.IsNullOrEmpty(dwgDir))
                {
                    if (dwgDir.EndsWith("\\" + targetFolderName, StringComparison.OrdinalIgnoreCase) ||
                        string.Equals(Path.GetFileName(dwgDir), targetFolderName, StringComparison.OrdinalIgnoreCase))
                        return true;
                }
            }

            // 4. Kiem tra ten file DWG co thuc su nam trong thu muc duoc chon (ke ca thu muc con)
            string fn = Path.GetFileName(dwgPath);
            if (!string.IsNullOrEmpty(fn) && folderDwgNames != null && folderDwgNames.Contains(fn))
            {
                return true;
            }

            return false;
        }

        public static List<CadastralRecord> DoSearchCore(string folderPath, string rawQuery, int mode, string layerFilter, int maxResults)
        {
            var results = new List<CadastralRecord>();
            if (string.IsNullOrEmpty(rawQuery)) return results;

            string qUni, qAsc;
            DecodeCadText(rawQuery, "Standard", out qUni, out qAsc);
            qUni = (qUni ?? "").Trim();
            qAsc = (qAsc ?? "").Trim();

            string normKeyword = NormalizeText(!string.IsNullOrEmpty(qUni) ? qUni : rawQuery);
            if (string.IsNullOrEmpty(normKeyword)) normKeyword = NormalizeText(qAsc);
            if (string.IsNullOrEmpty(normKeyword)) return results;

            string[] tokens = normKeyword.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (tokens.Length == 0) return results;

            string cleanFolder = (folderPath ?? "").Trim().TrimEnd('\\', '/');
            bool searchAllIndexed = string.IsNullOrEmpty(cleanFolder) ||
                                    string.Equals(cleanFolder, "D:", StringComparison.OrdinalIgnoreCase) ||
                                    string.Equals(cleanFolder, "D:\\", StringComparison.OrdinalIgnoreCase) ||
                                    string.Equals(cleanFolder, "ALL", StringComparison.OrdinalIgnoreCase);
            string normFolder = cleanFolder + "\\";

            bool hasLayerFilter = !string.IsNullOrEmpty(layerFilter) && layerFilter != "*" && !layerFilter.StartsWith("--") && !layerFilter.Contains("(*)");
            string layFilterUp = hasLayerFilter ? layerFilter.ToUpperInvariant() : "";

            var dwgMap = GetOrBuildDwgMap();
            if (dwgMap.Count == 0 || !File.Exists(IndexDbPath)) return results;

            var folderDwgNames = GetDwgsInFolder(cleanFolder);

            Action<bool> runScan = null;
            runScan = (scanAll) =>
            {
                List<DwgBlock> targets = new List<DwgBlock>();
                for (int b = 0; b < dwgMap.Count; b++)
                {
                    var blk = dwgMap[b];
                    if (blk.IsTongGhep) continue;
                    if (scanAll || PathMatchesFolder(blk.DwgPath, cleanFolder, folderDwgNames))
                    {
                        targets.Add(blk);
                    }
                }

                if (targets.Count == 0)
                {
                    // TUYET DOI KHONG tu y quet sang khu vuc khac khi nguoi dung da chi dinh thu muc!
                    return;
                }

                int cores = Math.Min(Environment.ProcessorCount, 16);
                if (cores < 1) cores = 1;

                var localMatches = new List<CadastralRecord>[cores];
                for (int c = 0; c < cores; c++) localMatches[c] = new List<CadastralRecord>();

                using (var mmf = MemoryMappedFile.CreateFromFile(IndexDbPath, FileMode.Open, null, 0, MemoryMappedFileAccess.Read))
                using (var va = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read))
                {
                    unsafe
                    {
                        byte* basePtr = null;
                        va.SafeMemoryMappedViewHandle.AcquirePointer(ref basePtr);
                        try
                        {
                            Parallel.For(0, targets.Count, new ParallelOptions { MaxDegreeOfParallelism = cores }, targetIdx =>
                            {
                                int threadId = targetIdx % cores;
                                var blk = targets[targetIdx];
                                byte* ptr = basePtr + blk.Offset;
                                byte* end = ptr + blk.Length;

                                while (ptr < end)
                                {
                                    byte* lineStart = ptr;
                                    while (ptr < end && *ptr != (byte)'\n') ptr++;
                                    int lineLen = (int)(ptr - lineStart);
                                    if (lineLen > 0 && *(ptr - 1) == (byte)'\r') lineLen--;

                                    if (lineLen > 0)
                                    {
                                        string line = Encoding.UTF8.GetString(lineStart, lineLen);
                                        string[] parts = line.Split('\t');
                                        if (parts.Length >= 6)
                                        {
                                            string dwgFile = blk.DwgPath;
                                            string layer = parts[1];
                                            string entType = parts[2];
                                            string xStr = parts[3];
                                            string yStr = parts[4];
                                            string uniContent = parts[5];
                                            string ascContent = parts.Length >= 7 ? parts[6] : "";

                                            if (hasLayerFilter && !WildcardMatch(layer.ToUpperInvariant(), layFilterUp))
                                            {
                                                ptr++;
                                                continue;
                                            }

                                            string normRaw = NormalizeText(uniContent);
                                            string normAsc = NormalizeText(ascContent);

                                            bool isMatch = false;

                                            if (mode == 0) // Mode 1: Khớp tất cả từ (AND)
                                            {
                                                bool matchAll = true;
                                                for (int i = 0; i < tokens.Length; i++)
                                                {
                                                    string tok = tokens[i];
                                                    bool hasNum = HasNumber(tok);
                                                    if (hasNum)
                                                    {
                                                        if (!HasWordBoundaryMatch(normRaw, tok) && !HasWordBoundaryMatch(normAsc, tok))
                                                        {
                                                            matchAll = false;
                                                            break;
                                                        }
                                                    }
                                                    else
                                                    {
                                                        if (tok.Length <= 2)
                                                        {
                                                            if (!HasWordBoundaryMatch(normRaw, tok) && !HasWordBoundaryMatch(normAsc, tok))
                                                            {
                                                                matchAll = false;
                                                                break;
                                                            }
                                                        }
                                                        else
                                                        {
                                                            if (!normRaw.Contains(tok) && !normAsc.Contains(tok))
                                                            {
                                                                matchAll = false;
                                                                break;
                                                            }
                                                        }
                                                    }
                                                }
                                                isMatch = matchAll;
                                            }
                                            else if (mode == 1) // Mode 2: Khớp chính xác cụm từ
                                            {
                                                isMatch = normRaw.Contains(normKeyword) || (!string.IsNullOrEmpty(normAsc) && normAsc.Contains(normKeyword));
                                            }
                                            else if (mode == 2) // Mode 3: Chỉ tìm Số GCN / Số Thửa
                                            {
                                                CadastralRecord rQuick = ParseCadastralRecord(dwgFile, layer, entType, xStr, yStr, uniContent);
                                                string gcnNorm = NormalizeText(rQuick.SoGcnNgayKy);
                                                string thuaNorm = NormalizeText(rQuick.ThuaLoai);
                                                string layUp = (layer ?? "").ToUpperInvariant();
                                                bool inGcnOrThuaLay = (layUp.Contains("SO GCN") || layUp.Contains("SO THUA") || layUp.Contains("GCN") || layUp.Contains("THUA"));

                                                bool matchGcn = true;
                                                bool matchThua = true;
                                                for (int i = 0; i < tokens.Length; i++)
                                                {
                                                    string tok = tokens[i];
                                                    bool hasNum = HasNumber(tok);
                                                    if (hasNum)
                                                    {
                                                        if (!HasWordBoundaryMatch(gcnNorm, tok)) matchGcn = false;
                                                        if (!HasWordBoundaryMatch(thuaNorm, tok)) matchThua = false;
                                                    }
                                                    else
                                                    {
                                                        if (!gcnNorm.Contains(tok)) matchGcn = false;
                                                        if (!thuaNorm.Contains(tok)) matchThua = false;
                                                    }
                                                }
                                                if (matchGcn || matchThua)
                                                {
                                                    isMatch = true;
                                                }
                                                else if (inGcnOrThuaLay)
                                                {
                                                    bool matchAll = true;
                                                    for (int i = 0; i < tokens.Length; i++)
                                                    {
                                                        string tok = tokens[i];
                                                        if (HasNumber(tok))
                                                        {
                                                            if (!HasWordBoundaryMatch(normRaw, tok) && !HasWordBoundaryMatch(normAsc, tok)) { matchAll = false; break; }
                                                        }
                                                        else
                                                        {
                                                            if (!normRaw.Contains(tok) && !normAsc.Contains(tok)) { matchAll = false; break; }
                                                        }
                                                    }
                                                    if (matchAll) isMatch = true;
                                                }
                                            }

                                            if (isMatch)
                                            {
                                                var rec = ParseCadastralRecord(dwgFile, layer, entType, xStr, yStr, uniContent);
                                                localMatches[threadId].Add(rec);
                                            }
                                        }
                                    }
                                    ptr++;
                                }
                            });
                        }
                        finally
                        {
                            va.SafeMemoryMappedViewHandle.ReleasePointer();
                        }
                    }
                }

                for (int c = 0; c < cores; c++)
                {
                    results.AddRange(localMatches[c]);
                    if (results.Count >= maxResults) break;
                }
            };

            runScan(searchAllIndexed);

            if (results.Count > maxResults)
            {
                results = results.GetRange(0, maxResults);
            }

            for (int i = 0; i < results.Count; i++)
            {
                results[i].Stt = i + 1;
                results[i].DwgPath = ResolveExistingDwgPath(results[i].DwgPath);
            }

            return results;
        }

        static unsafe void DoSearch(string folderPath, string rawQuery, string matchMode, string layerFilter, int maxResults, string outTsvPath)
        {
            Directory.CreateDirectory(WorkDir);
            int mode = 0;
            int.TryParse(matchMode, out mode);

            var finalResults = DoSearchCore(folderPath, rawQuery, mode, layerFilter, maxResults);
            var matchedDwgSet = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var r in finalResults) matchedDwgSet.Add(r.DwgPath);

            using (var sw = new StreamWriter(outTsvPath, false, Latin1))
            {
                sw.WriteLine(finalResults.Count + "\t" + matchedDwgSet.Count + "\t" + finalResults.Count);
                foreach (var r in finalResults)
                {
                    string dwgFull = EncodeUPlusForLisp(r.DwgPath);
                    string dwgName = EncodeUPlusForLisp(Path.GetFileName(r.DwgPath));
                    string parentDir = EncodeUPlusForLisp(r.ParentDir);
                    string layer = r.Layer;
                    string entType = r.EntType;
                    string xStr = r.XStr;
                    string yStr = r.YStr;
                    string dispUniLisp = EncodeUPlusForLisp(r.RawContent);
                    sw.WriteLine(string.Join("\t", new string[] {
                        dwgFull, dwgName, parentDir, layer, entType, xStr, yStr, dispUniLisp
                    }));
                }
            }

            try
            {
                string reportPath = Path.Combine(WorkDir, "BAO_CAO_TIM_KIEM_THU_MUC.txt");
                using (var rsw = new StreamWriter(reportPath, false, Utf8Bom))
                {
                    rsw.WriteLine("================================================================================");
                    rsw.WriteLine("       BAO CAO KET QUA TIM KIEM TREN TOAN BO THU MUC BAN DO CAD");
                    rsw.WriteLine("================================================================================");
                    rsw.WriteLine("Thu muc quet : " + folderPath);
                    rsw.WriteLine("Tu khoa tim  : " + rawQuery);
                    rsw.WriteLine("Tong ket qua : " + finalResults.Count + " doi tuong tai " + matchedDwgSet.Count + " to ban do (.dwg)");
                    rsw.WriteLine("--------------------------------------------------------------------------------");
                    int idx = 1;
                    foreach (var r in finalResults)
                    {
                        rsw.WriteLine(string.Format("[{0}] To: {1} ({2}) | Layer: {3} | Toa do: X={4}, Y={5}",
                            idx++, r.DwgName, r.ParentDir, r.Layer, r.XStr, r.YStr));
                        rsw.WriteLine("    Noi dung: " + r.RawContent);
                        rsw.WriteLine("    Duong dan: " + r.DwgPath);
                    }
                    rsw.WriteLine("================================================================================");
                }

                string csvPath = Path.Combine(WorkDir, "KET_QUA_TIM_KIEM_THU_MUC.csv");
                using (var csw = new StreamWriter(csvPath, false, Utf8Bom))
                {
                    csw.WriteLine("STT,TenToBanDo,ThuaLoai,ChuSuDungDiaChi,SoGcnNgayKy,Layer,Loai,ToaDo_X,ToaDo_Y,DuongDanDayDu");
                    int idx = 1;
                    foreach (var r in finalResults)
                    {
                        string safeThua = "\"" + (r.ThuaLoai ?? "").Replace("\"", "\"\"") + "\"";
                        string safeChu = "\"" + (r.ChuSuDungDiaChi ?? "").Replace("\"", "\"\"") + "\"";
                        string safeGcn = "\"" + (r.SoGcnNgayKy ?? "").Replace("\"", "\"\"") + "\"";
                        string safePath = "\"" + (r.DwgPath ?? "").Replace("\"", "\"\"") + "\"";
                        csw.WriteLine(string.Format("{0},\"{1}\",{2},{3},{4},\"{5}\",\"{6}\",{7},{8},{9}",
                            idx++, r.DwgName, safeThua, safeChu, safeGcn, r.Layer, r.EntType, r.XStr, r.YStr, safePath));
                    }
                }
            }
            catch { }
        }

        // =========================================================================
        // WINFORMS FORM: Tra Cứu Bản Đồ Địa Chính Siêu Tốc - Fast Find Pro v13.0
        // =========================================================================
        public class FastFindForm : Form
        {
            [DllImport("user32.dll")]
            static extern bool SetForegroundWindow(IntPtr hWnd);

            [DllImport("user32.dll")]
            static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);

            TextBox txtKeyword;
            Button btnSearch;
            LinkLabel lnkFolder;
            Button btnBrowse;
            RadioButton rbAnd, rbExact, rbGcnThua;
            TextBox txtSubFilter;
            Label lblResultCount;
            ListView lvResults;
            Label lblDetailContent, lblDetailSheet, lblDetailCoords;
            Button btnCopyCadCoords;
            Button btnOpen, btnInsert, btnExcel, btnReport, btnReset, btnClose;
            Button btnUpdateNotice;
            string remoteVersionFound = null;

            public static string ActiveSearchFolder = "";
            string currentFolder = "";
            List<CadastralRecord> allResults = new List<CadastralRecord>();
            List<CadastralRecord> visibleResults = new List<CadastralRecord>();

            public FastFindForm(string initialFolder, string initialKeyword)
            {
                currentFolder = (initialFolder ?? "").Trim();
                if (string.IsNullOrEmpty(currentFolder) || (!currentFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase) && !Directory.Exists(currentFolder)))
                {
                    string saved = LoadLastFolder();
                    if (!string.IsNullOrEmpty(saved) && (saved.Equals("ALL", StringComparison.OrdinalIgnoreCase) || Directory.Exists(saved)))
                    {
                        currentFolder = saved;
                    }
                    else
                    {
                        // Mac dinh quet toan bo CSDL khi cai o may moi
                        currentFolder = "ALL";
                    }
                }
                else
                {
                    SaveLastFolder(currentFolder);
                }
                ActiveSearchFolder = currentFolder;
                InitializeComponent();
                UpdateFolderDisplay();
                Task.Factory.StartNew(() => CheckUpdateFromGitHub());

                if (!string.IsNullOrEmpty(initialKeyword))
                {
                    txtKeyword.Text = initialKeyword;
                    this.Shown += (s, e) => ExecuteSearch();
                }
            }

            void InitializeComponent()
            {
                this.Text = "CadaPro-CAD v1.0 - Tra Cứu Bản Đồ Địa Chính Siêu Tốc";
                try
                {
                    string iconPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "app_icon.ico");
                    if (File.Exists(iconPath)) this.Icon = new Icon(iconPath);
                    else this.Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath);
                }
                catch { }
                this.Size = new Size(1180, 780);
                this.MinimumSize = new Size(1000, 650);
                this.StartPosition = FormStartPosition.CenterScreen;
                this.Font = new Font("Segoe UI", 9.25f, FontStyle.Regular);
                this.BackColor = Color.FromArgb(248, 249, 250);
                this.KeyPreview = true;
                this.ShowInTaskbar = true;
                this.Shown += (s, e) =>
                {
                    this.Activate();
                    this.BringToFront();
                };

                // 1. Top Panel (Dock = Top, Height = 120)
                Panel pnlTop = new Panel();
                pnlTop.Dock = DockStyle.Top;
                pnlTop.Height = 120;
                pnlTop.BackColor = Color.White;
                pnlTop.BorderStyle = BorderStyle.FixedSingle;

                Label lblKw = new Label() { Text = "🔍 Từ khóa :", Location = new Point(12, 14), AutoSize = true, Font = new Font("Segoe UI", 9.5f, FontStyle.Bold) };
                txtKeyword = new TextBox() { Location = new Point(95, 11), Size = new Size(310, 26), Font = new Font("Segoe UI", 10.5f, FontStyle.Regular) };
                btnSearch = new Button() { Text = "TÌM KIẾM (Enter)", Location = new Point(415, 10), Size = new Size(140, 29), BackColor = Color.FromArgb(13, 110, 253), ForeColor = Color.White, Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.Flat };

                Label lblFolderTitle = new Label() { Text = "📁 Thư mục:", Location = new Point(555, 15), AutoSize = true, Font = new Font("Segoe UI", 9.5f, FontStyle.Bold) };
                
                btnBrowse = new Button()
                {
                    Text = "Đổi Thư Mục...",
                    Size = new Size(100, 28),
                    FlatStyle = FlatStyle.System,
                    Anchor = AnchorStyles.Top | AnchorStyles.Right,
                    Location = new Point(pnlTop.ClientSize.Width - 110, 10)
                };

                Button btnAllDb = new Button()
                {
                    Text = "Toàn Bộ CSDL",
                    Size = new Size(95, 28),
                    FlatStyle = FlatStyle.System,
                    Anchor = AnchorStyles.Top | AnchorStyles.Right,
                    Location = new Point(pnlTop.ClientSize.Width - 210, 10)
                };

                lnkFolder = new LinkLabel()
                {
                    Text = currentFolder,
                    Location = new Point(635, 15),
                    AutoEllipsis = true,
                    Font = new Font("Segoe UI", 9.25f, FontStyle.Regular),
                    Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right,
                    Width = pnlTop.ClientSize.Width - 635 - 215
                };

                pnlTop.Controls.AddRange(new Control[] { lblKw, txtKeyword, btnSearch, lblFolderTitle, lnkFolder, btnAllDb, btnBrowse });

                // Row 2: Modes
                Label lblMode = new Label() { Text = "⚙️ Chế độ  :", Location = new Point(12, 48), AutoSize = true, Font = new Font("Segoe UI", 9.5f, FontStyle.Bold) };
                rbAnd = new RadioButton() { Text = "Khớp tất cả từ (AND)", Location = new Point(95, 47), AutoSize = true, Checked = true, Font = new Font("Segoe UI", 9.25f, FontStyle.Regular) };
                rbExact = new RadioButton() { Text = "Khớp chính xác cụm từ", Location = new Point(270, 47), AutoSize = true, Font = new Font("Segoe UI", 9.25f, FontStyle.Regular) };
                rbGcnThua = new RadioButton() { Text = "Chỉ tìm Số GCN / Số Thửa", Location = new Point(460, 47), AutoSize = true, Font = new Font("Segoe UI", 9.25f, FontStyle.Regular) };

                                btnUpdateNotice = new Button()
                {
                    Text = "🔔 Có bản mới! [Cập Nhật Ngay]",
                    Size = new Size(260, 28),
                    Location = new Point(pnlTop.ClientSize.Width - 365, 46),
                    Anchor = AnchorStyles.Top | AnchorStyles.Right,
                    BackColor = Color.FromArgb(253, 126, 20),
                    ForeColor = Color.White,
                    Font = new Font("Segoe UI", 9f, FontStyle.Bold),
                    FlatStyle = FlatStyle.Flat,
                    Cursor = Cursors.Hand,
                    Visible = false
                };
                btnUpdateNotice.FlatAppearance.BorderSize = 0;
                btnUpdateNotice.Click += (s, e) => HandleUpdateClick();
                pnlTop.Controls.AddRange(new Control[] { lblMode, rbAnd, rbExact, rbGcnThua, btnUpdateNotice });

                // Row 3: Subfilter
                Label lblSub = new Label() { Text = "⚡ Lọc phụ :", Location = new Point(12, 82), AutoSize = true, Font = new Font("Segoe UI", 9.5f, FontStyle.Bold) };
                txtSubFilter = new TextBox() { Location = new Point(95, 79), Size = new Size(520, 26), Font = new Font("Segoe UI", 9.5f, FontStyle.Regular) };
                lblResultCount = new Label() { Text = "✅ Tìm thấy: 0 kết quả", Location = new Point(630, 82), AutoSize = true, Font = new Font("Segoe UI", 9.5f, FontStyle.Bold), ForeColor = Color.FromArgb(25, 135, 84) };

                pnlTop.Controls.AddRange(new Control[] { lblSub, txtSubFilter, lblResultCount });

                // 2. Details Panel (Dock = Bottom, sitting above Actions)
                Panel pnlDetails = new Panel();
                pnlDetails.Dock = DockStyle.Bottom;
                pnlDetails.Height = 125;
                pnlDetails.BackColor = Color.White;
                pnlDetails.Padding = new Padding(12, 4, 12, 4);

                GroupBox gbDetails = new GroupBox()
                {
                    Text = "THÔNG TIN CHI TIẾT ĐỐI TƯỢNG ĐANG CHỌN (TỌA ĐỘ VN-2000):",
                    Dock = DockStyle.Fill,
                    Font = new Font("Segoe UI", 9f, FontStyle.Bold),
                    ForeColor = Color.FromArgb(33, 37, 41)
                };
                pnlDetails.Controls.Add(gbDetails);

                lblDetailContent = new Label()
                {
                    Text = "📌 Nội dung gốc : (Chưa chọn đối tượng)",
                    Location = new Point(15, 23),
                    Size = new Size(1120, 22),
                    AutoEllipsis = true,
                    Font = new Font("Segoe UI", 9.25f, FontStyle.Regular),
                    Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right
                };

                lblDetailSheet = new Label()
                {
                    Text = "🗺️ Tờ bản đồ    : ---   | Thư mục: ---   | Layer: ---   | Loại: ---",
                    Location = new Point(15, 48),
                    Size = new Size(1120, 22),
                    Font = new Font("Segoe UI", 9.25f, FontStyle.Regular),
                    Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right
                };

                lblDetailCoords = new Label()
                {
                    Text = "🎯 Tọa độ VN2000: X = 0.00 , Y = 0.00  --> ",
                    Location = new Point(15, 75),
                    AutoSize = true,
                    Font = new Font("Segoe UI", 9.25f, FontStyle.Regular)
                };

                btnCopyCadCoords = new Button()
                {
                    Text = "📋 Copy Tọa Độ Chuẩn CAD: 0.00,0.00",
                    Location = new Point(365, 70),
                    Size = new Size(410, 32),
                    BackColor = Color.Gray,
                    ForeColor = Color.White,
                    Font = new Font("Segoe UI", 9f, FontStyle.Bold),
                    FlatStyle = FlatStyle.Flat,
                    Enabled = false
                };

                gbDetails.Controls.AddRange(new Control[] { lblDetailContent, lblDetailSheet, lblDetailCoords, btnCopyCadCoords });

                // 3. Bottom Actions Panel (Dock = Bottom, outermost bottom)
                Panel pnlActions = new Panel();
                pnlActions.Dock = DockStyle.Bottom;
                pnlActions.Height = 56;
                pnlActions.BackColor = Color.FromArgb(241, 243, 245);

                btnOpen = new Button() { Text = "🚀 Mở Tờ BĐ & Phóng Tới (Enter)", Location = new Point(12, 10), Size = new Size(230, 36), BackColor = Color.FromArgb(13, 110, 253), ForeColor = Color.White, Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.Flat };
                btnInsert = new Button() { Text = "📥 Chèn Vào Bản Vẽ Đang Mở", Location = new Point(250, 10), Size = new Size(205, 36), BackColor = Color.FromArgb(23, 162, 184), ForeColor = Color.White, Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.Flat };
                btnExcel = new Button() { Text = "📊 Xuất Excel", Location = new Point(465, 10), Size = new Size(130, 36), BackColor = Color.FromArgb(40, 167, 69), ForeColor = Color.White, Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.Flat };
                btnReport = new Button() { Text = "📄 Mở Báo Cáo", Location = new Point(605, 10), Size = new Size(130, 36), Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.System };
                btnReset = new Button() { Text = "🔄 Đặt Lại Tìm Kiếm", Location = new Point(745, 10), Size = new Size(160, 36), Font = new Font("Segoe UI", 9f, FontStyle.Bold), FlatStyle = FlatStyle.System };
                btnClose = new Button()
                {
                    Text = "❌ Đóng (Esc)",
                    Size = new Size(130, 36),
                    Font = new Font("Segoe UI", 9f, FontStyle.Bold),
                    FlatStyle = FlatStyle.System,
                    Anchor = AnchorStyles.Top | AnchorStyles.Right,
                    Location = new Point(pnlActions.ClientSize.Width - 145, 10)
                };

                pnlActions.Controls.AddRange(new Control[] { btnOpen, btnInsert, btnExcel, btnReport, btnReset, btnClose });

                // 4. Center Panel (ListView)
                Panel pnlCenter = new Panel();
                pnlCenter.Dock = DockStyle.Fill;
                pnlCenter.Padding = new Padding(12, 6, 12, 6);

                Label lblListHeader = new Label()
                {
                    Text = "DANH SÁCH KẾT QUẢ TÌM THẤY (Bấm phím Mũi Tên để duyệt - Nhấp đúp hoặc Enter để Mở & Zoom):",
                    Dock = DockStyle.Top,
                    Height = 26,
                    Font = new Font("Segoe UI", 9.25f, FontStyle.Bold),
                    ForeColor = Color.FromArgb(33, 37, 41)
                };

                lvResults = new ListView();
                lvResults.Dock = DockStyle.Fill;
                lvResults.View = View.Details;
                lvResults.FullRowSelect = true;
                lvResults.GridLines = true;
                lvResults.MultiSelect = false;
                lvResults.Font = new Font("Segoe UI", 9.5f, FontStyle.Regular);
                lvResults.HeaderStyle = ColumnHeaderStyle.Clickable;

                lvResults.Columns.Add("STT", 50, HorizontalAlignment.Center);
                lvResults.Columns.Add("Tờ Bản Đồ", 115);
                lvResults.Columns.Add("Thửa / Loại", 130);
                lvResults.Columns.Add("Chủ Sử Dụng / Địa Chỉ", 350);
                lvResults.Columns.Add("Số GCN / Ngày Ký", 180);
                lvResults.Columns.Add("Layer", 120);

                typeof(ListView).InvokeMember("DoubleBuffered",
                    BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.SetProperty,
                    null, lvResults, new object[] { true });

                pnlCenter.Controls.Add(lvResults);
                pnlCenter.Controls.Add(lblListHeader);
                lblListHeader.BringToFront();
                lvResults.BringToFront();

                // ADD CONTROLS TO FORM & SET EXACT DOCKING Z-ORDER
                this.Controls.Add(pnlTop);
                this.Controls.Add(pnlDetails);
                this.Controls.Add(pnlActions);
                this.Controls.Add(pnlCenter);

                pnlActions.BringToFront();
                pnlDetails.BringToFront();
                pnlCenter.BringToFront();

                // Auto stretch column 3 on resize
                this.Resize += (s, e) =>
                {
                    int totalW = lvResults.ClientSize.Width;
                    if (totalW > 800 && lvResults.Columns.Count >= 6)
                    {
                        int fixedW = 50 + 115 + 130 + 180 + 120 + 25;
                        int rest = totalW - fixedW;
                        if (rest > 250) lvResults.Columns[3].Width = rest;
                    }
                };

                // Events
                btnSearch.Click += (s, e) => ExecuteSearch();
                txtKeyword.KeyDown += (s, e) => { if (e.KeyCode == Keys.Enter) { e.SuppressKeyPress = true; ExecuteSearch(); } };

                rbAnd.CheckedChanged += (s, e) => { if (rbAnd.Checked && allResults.Count > 0) ExecuteSearch(); };
                rbExact.CheckedChanged += (s, e) => { if (rbExact.Checked && allResults.Count > 0) ExecuteSearch(); };
                rbGcnThua.CheckedChanged += (s, e) => { if (rbGcnThua.Checked && allResults.Count > 0) ExecuteSearch(); };

                txtSubFilter.TextChanged += (s, e) => ApplySubFilter();

                btnBrowse.Click += (s, e) => BrowseFolder();
                btnAllDb.Click += (s, e) =>
                {
                    currentFolder = "ALL";
                    ActiveSearchFolder = "ALL";
                    SaveLastFolder("ALL");
                    UpdateFolderDisplay();
                    if (!string.IsNullOrEmpty(txtKeyword.Text.Trim()))
                    {
                        ExecuteSearch();
                    }
                };
                lnkFolder.LinkClicked += (s, e) => BrowseFolder();

                lvResults.SelectedIndexChanged += (s, e) => UpdateDetailsPanel();
                lvResults.DoubleClick += (s, e) => DoOpenAction();
                lvResults.KeyDown += (s, e) =>
                {
                    if (e.KeyCode == Keys.Enter)
                    {
                        e.SuppressKeyPress = true;
                        DoOpenAction();
                    }
                };

                btnCopyCadCoords.Click += (s, e) => CopyCoordsToClipboard();

                btnOpen.Click += (s, e) => DoOpenAction();
                btnInsert.Click += (s, e) => DoInsertAction();
                btnExcel.Click += (s, e) => ExportExcel();
                btnReport.Click += (s, e) => OpenReport();
                btnReset.Click += (s, e) => ResetForm();
                btnClose.Click += (s, e) => this.Close();

                this.KeyDown += (s, e) =>
                {
                    if (e.KeyCode == Keys.Escape) this.Close();
                };
            }

            void UpdateFolderDisplay()
            {
                ActiveSearchFolder = currentFolder;
                if (string.IsNullOrEmpty(currentFolder) || currentFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase))
                {
                    lnkFolder.Text = "[TOÀN BỘ CƠ SỞ DỮ LIỆU BẢN ĐỒ] (Không giới hạn thư mục)";
                    lnkFolder.LinkColor = Color.DarkGreen;
                }
                else
                {
                    lnkFolder.Text = currentFolder;
                    lnkFolder.LinkColor = Color.FromArgb(13, 110, 253);
                }
            }

            void CheckUpdateFromGitHub()
            {
                try
                {
                    string remoteVer = FetchRemoteVersion();
                    if (!string.IsNullOrEmpty(remoteVer) && IsNewerVersion(remoteVer, CurrentVersion))
                    {
                        remoteVersionFound = remoteVer;
                        try
                        {
                            string noticePath = Path.Combine(WorkDir, "new_update_notice.txt");
                            File.WriteAllText(noticePath, remoteVer);
                        }
                        catch { }

                        this.BeginInvoke((Action)(() =>
                        {
                            btnUpdateNotice.Text = string.Format("🔔 Có bản mới v{0} [Cập Nhật Ngay]", remoteVer);
                            btnUpdateNotice.Visible = true;
                        }));
                    }
                }
                catch { }
            }

            void HandleUpdateClick()
            {
                string ver = remoteVersionFound ?? "mới nhất";
                var dr = MessageBox.Show(
                    string.Format("Đã có phiên bản mới ({0}) trên GitHub!\nBạn có muốn tự động tải về và nâng cấp ngay bây giờ không?", ver),
                    "CadaPro-CAD - Cập Nhật Phiên Bản",
                    MessageBoxButtons.YesNo,
                    MessageBoxIcon.Information);

                if (dr == DialogResult.Yes)
                {
                    btnUpdateNotice.Text = "⏳ Đang tải bản mới từ GitHub...";
                    btnUpdateNotice.Enabled = false;

                    Task.Factory.StartNew(() =>
                    {
                        bool ok = PerformDirectUpdate();
                        this.BeginInvoke((Action)(() =>
                        {
                            if (ok)
                            {
                                MessageBox.Show(
                                    "Đã cập nhật thành công lên bản " + ver + "!\nVui lòng khởi động lại ứng dụng để áp dụng tính năng mới.",
                                    "CadaPro-CAD - Thành Công",
                                    MessageBoxButtons.OK,
                                    MessageBoxIcon.Information);
                                btnUpdateNotice.Visible = false;
                            }
                            else
                            {
                                MessageBox.Show(
                                    "Không thể tải bản cập nhật tự động. Vui lòng kiểm tra kết nối mạng hoặc gõ lệnh CAPNHAT trong CAD.",
                                    "Lỗi Cập Nhật",
                                    MessageBoxButtons.OK,
                                    MessageBoxIcon.Warning);
                                btnUpdateNotice.Text = "🔔 Có bản mới! [Thử Lại]";
                                btnUpdateNotice.Enabled = true;
                            }
                        }));
                    });
                }
            }

            void BrowseFolder()
            {
                using (var fbd = new FolderBrowserDialog())
                {
                    fbd.Description = "Chọn thư mục chứa các tờ bản đồ địa chính (.DWG) cần tra cứu trên máy này:";
                    fbd.SelectedPath = (Directory.Exists(currentFolder) && !currentFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase))
                        ? currentFolder
                        : (Directory.Exists(@"D:\") ? @"D:\" : @"C:\");
                    fbd.ShowNewFolderButton = false;
                    if (fbd.ShowDialog(this) == DialogResult.OK)
                    {
                        currentFolder = fbd.SelectedPath;
                        ActiveSearchFolder = currentFolder;
                        lock (FolderDwgCache) { FolderDwgCache.Remove(currentFolder); }
                        SaveLastFolder(currentFolder);
                        UpdateFolderDisplay();
                        if (!string.IsNullOrEmpty(txtKeyword.Text.Trim()))
                        {
                            ExecuteSearch();
                        }
                    }
                }
            }

            void ExecuteSearch()
            {
                string kw = txtKeyword.Text.Trim();
                if (string.IsNullOrEmpty(kw))
                {
                    txtKeyword.Focus();
                    return;
                }

                int mode = 0;
                if (rbExact.Checked) mode = 1;
                else if (rbGcnThua.Checked) mode = 2;

                btnSearch.Enabled = false;
                lblResultCount.Text = "⏳ Đang quét cơ sở dữ liệu địa chính...";
                lblResultCount.ForeColor = Color.FromArgb(13, 110, 253);
                Cursor.Current = Cursors.WaitCursor;

                Task.Factory.StartNew(() =>
                {
                    return DoSearchCore(currentFolder, kw, mode, "*", 500);
                }).ContinueWith(t =>
                {
                    this.BeginInvoke((Action)(() =>
                    {
                        btnSearch.Enabled = true;
                        Cursor.Current = Cursors.Default;

                        if (t.IsFaulted || t.Result == null)
                        {
                            allResults = new List<CadastralRecord>();
                        }
                        else
                        {
                            allResults = t.Result;
                        }

                        ApplySubFilter();

                        if (allResults.Count == 0)
                        {
                            if (!string.IsNullOrEmpty(currentFolder) && !currentFolder.Equals("ALL", StringComparison.OrdinalIgnoreCase))
                            {
                                lblResultCount.Text = "❌ Không có bản vẽ nào trong thư mục này khớp dữ liệu (Bấm [Toàn Bộ CSDL] để tìm tất cả)";
                                lblResultCount.ForeColor = Color.FromArgb(220, 53, 69);
                            }
                        }

                        // Write to folder_res.tsv and csv automatically
                        try
                        {
                            string outTsv = Path.Combine(WorkDir, "folder_res.tsv");
                            DoSearch(currentFolder, kw, mode.ToString(), "*", 500, outTsv);
                        }
                        catch { }
                    }));
                });
            }

            void ApplySubFilter()
            {
                string subText = txtSubFilter.Text.Trim();
                if (string.IsNullOrEmpty(subText))
                {
                    visibleResults = new List<CadastralRecord>(allResults);
                }
                else
                {
                    string normSub = NormalizeText(subText);
                    string[] subTokens = normSub.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

                    visibleResults = new List<CadastralRecord>();
                    foreach (var rec in allResults)
                    {
                        bool pass = true;
                        string fullSearchable = NormalizeText(rec.RawContent) + " " +
                                                NormalizeText(rec.ToBanDo) + " " +
                                                NormalizeText(rec.ChuSuDungDiaChi) + " " +
                                                NormalizeText(rec.Layer);
                        for (int i = 0; i < subTokens.Length; i++)
                        {
                            string tok = subTokens[i];
                            if (!fullSearchable.Contains(tok))
                            {
                                pass = false;
                                break;
                            }
                        }
                        if (pass) visibleResults.Add(rec);
                    }
                }

                // Render to ListView
                lvResults.BeginUpdate();
                lvResults.Items.Clear();

                for (int i = 0; i < visibleResults.Count; i++)
                {
                    var rec = visibleResults[i];
                    string sttStr = string.Format("{0:D3}", i + 1);
                    var item = new ListViewItem(sttStr);
                    item.SubItems.Add(rec.ToBanDo);
                    item.SubItems.Add(rec.ThuaLoai);
                    item.SubItems.Add(rec.ChuSuDungDiaChi);
                    item.SubItems.Add(rec.SoGcnNgayKy);
                    item.SubItems.Add(rec.Layer);
                    item.Tag = rec;
                    lvResults.Items.Add(item);
                }

                lvResults.EndUpdate();

                if (visibleResults.Count > 0)
                {
                    lblResultCount.Text = string.Format("✅ Tìm thấy: {0} kết quả", visibleResults.Count);
                    if (visibleResults.Count != allResults.Count)
                    {
                        lblResultCount.Text = string.Format("✅ Lọc thấy: {0} / {1} kết quả", visibleResults.Count, allResults.Count);
                    }
                    lblResultCount.ForeColor = Color.FromArgb(25, 135, 84);
                    lvResults.Items[0].Selected = true;
                    lvResults.Items[0].Focused = true;
                }
                else
                {
                    if (allResults.Count == 0)
                    {
                        lblResultCount.Text = "⚠️ Không tìm thấy kết quả phù hợp!";
                    }
                    else
                    {
                        lblResultCount.Text = string.Format("⚠️ 0 kết quả lọc (trong tổng số {0})", allResults.Count);
                    }
                    lblResultCount.ForeColor = Color.FromArgb(220, 53, 69);
                    ResetDetailsPanel();
                }
            }

            void UpdateDetailsPanel()
            {
                if (lvResults.SelectedItems.Count == 0)
                {
                    ResetDetailsPanel();
                    return;
                }

                var rec = lvResults.SelectedItems[0].Tag as CadastralRecord;
                if (rec == null) return;

                lblDetailContent.Text = "📌 Nội dung gốc : \"" + rec.RawContent + "\"";
                lblDetailSheet.Text = string.Format("🗺️ Tờ bản đồ    : {0}   | Thư mục: {1}   | Layer: {2}   | Loại: {3}",
                    rec.DwgName, rec.ParentDir, rec.Layer, rec.EntType);
                lblDetailCoords.Text = string.Format("🎯 Tọa độ VN2000: X = {0} , Y = {1}  --> ", rec.XStr, rec.YStr);
                btnCopyCadCoords.Text = string.Format("📋 Copy Tọa Độ Chuẩn CAD: {0}", rec.CoordCad);
                btnCopyCadCoords.Enabled = true;
                btnCopyCadCoords.BackColor = Color.FromArgb(25, 135, 84);
            }

            void ResetDetailsPanel()
            {
                lblDetailContent.Text = "📌 Nội dung gốc : (Chưa chọn đối tượng)";
                lblDetailSheet.Text = "🗺️ Tờ bản đồ    : ---   | Thư mục: ---   | Layer: ---   | Loại: ---";
                lblDetailCoords.Text = "🎯 Tọa độ VN2000: X = 0.00 , Y = 0.00  --> ";
                btnCopyCadCoords.Text = "📋 Copy Tọa Độ Chuẩn CAD: 0.00,0.00";
                btnCopyCadCoords.Enabled = false;
                btnCopyCadCoords.BackColor = Color.Gray;
            }

            void CopyCoordsToClipboard()
            {
                if (lvResults.SelectedItems.Count == 0) return;
                var rec = lvResults.SelectedItems[0].Tag as CadastralRecord;
                if (rec == null) return;

                Clipboard.SetText(rec.CoordCad);
                btnCopyCadCoords.Text = "✅ ĐÃ COPY: " + rec.CoordCad;
                btnCopyCadCoords.BackColor = Color.FromArgb(13, 110, 253);

                Timer t = new Timer();
                t.Interval = 2000;
                t.Tick += (s, e) =>
                {
                    t.Stop();
                    t.Dispose();
                    if (!btnCopyCadCoords.IsDisposed)
                    {
                        btnCopyCadCoords.Text = string.Format("📋 Copy Tọa Độ Chuẩn CAD: {0}", rec.CoordCad);
                        btnCopyCadCoords.BackColor = Color.FromArgb(25, 135, 84);
                    }
                };
                t.Start();
            }

            CadastralRecord GetSelectedRecord()
            {
                if (lvResults.SelectedItems.Count == 0) return null;
                return lvResults.SelectedItems[0].Tag as CadastralRecord;
            }

            void WriteActionFile(string action, CadastralRecord rec)
            {
                try
                {
                    Directory.CreateDirectory(WorkDir);
                    string actionFile = Path.Combine(WorkDir, "selected_action.tsv");
                    using (var sw = new StreamWriter(actionFile, false, Utf8Bom))
                    {
                        sw.WriteLine(action + "\t" + rec.DwgPath + "\t" + rec.XStr + "\t" + rec.YStr + "\t" + rec.CoordCad);
                    }
                }
                catch { }
            }

            void DoOpenAction()
            {
                var rec = GetSelectedRecord();
                if (rec == null)
                {
                    MessageBox.Show("Vui lòng chọn một kết quả trong danh sách trước!", "Thông Báo", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    return;
                }

                WriteActionFile("OPEN", rec);

                // Write coordinate jump files for AutoCAD automatic zoom upon opening
                try
                {
                    string pendingContent = rec.DwgPath + "\r\n" + rec.XStr + "\r\n" + rec.YStr + "\r\n" + rec.RawContent + "\r\n" + rec.Layer;
                    string jumpContent = rec.DwgPath + "\t" + rec.XStr + "\t" + rec.YStr + "\t" + rec.CoordCad;

                    string[] rootDirs = new string[] {
                        WorkDir,
                        @"D:\EXTRACUUAUTOCAD",
                        @"C:\EXTRACUUAUTOCAD",
                        @"D:\VBA",
                        @"C:\VBA"
                    };

                    foreach (var rd in rootDirs)
                    {
                        try
                        {
                            if (!Directory.Exists(rd)) Directory.CreateDirectory(rd);
                            File.WriteAllText(Path.Combine(rd, "pending_zoom.txt"), pendingContent, Encoding.UTF8);
                            File.WriteAllText(Path.Combine(rd, "fast_jump.tsv"), jumpContent, Utf8Bom);
                        }
                        catch { }
                    }
                }
                catch { }

                // Automatically copy standard CAD coordinates Y,X to Clipboard
                try { Clipboard.SetText(rec.CoordCad); } catch { }

                // 1. Try AutoCAD COM direct zoom
                bool comSuccess = false;
                try
                {
                    string[] progIds = new string[] {
                        "AutoCAD.Application.24.2",
                        "AutoCAD.Application.24.1",
                        "AutoCAD.Application.24",
                        "AutoCAD.Application"
                    };
                    object acadObj = null;
                    foreach (var pid in progIds)
                    {
                        try { acadObj = Marshal.GetActiveObject(pid); if (acadObj != null) break; } catch { }
                    }
                    if (acadObj != null)
                    {
                        dynamic acad = acadObj;
                        acad.Visible = true;
                        dynamic docs = acad.Documents;
                        dynamic targetDoc = null;
                        foreach (dynamic d in docs)
                        {
                            if (string.Equals((string)d.FullName, rec.DwgPath, StringComparison.OrdinalIgnoreCase) ||
                                string.Equals(Path.GetFileName((string)d.FullName), rec.DwgName, StringComparison.OrdinalIgnoreCase))
                            {
                                targetDoc = d;
                                break;
                            }
                        }
                        if (targetDoc == null)
                        {
                            targetDoc = docs.Open(rec.DwgPath);
                        }
                        targetDoc.Activate();
                        string sendCmd = string.Format(CultureInfo.InvariantCulture,
                            "(progn (if (fboundp 'TC-Check-Pending-Zoom) (TC-Check-Pending-Zoom) (command \"_.ZOOM\" \"_C\" (list {0} {1} 0.0) 40)) (princ)) \n",
                            rec.XStr, rec.YStr);
                        targetDoc.SendCommand(sendCmd);
                        comSuccess = true;
                    }
                }
                catch { }

                // 2. Bring AutoCAD to front if COM succeeded
                if (comSuccess)
                {
                    try
                    {
                        var acadProcs = Process.GetProcessesByName("acad");
                        if (acadProcs.Length > 0 && acadProcs[0].MainWindowHandle != IntPtr.Zero)
                        {
                            ShowWindow(acadProcs[0].MainWindowHandle, 9);
                            SetForegroundWindow(acadProcs[0].MainWindowHandle);
                        }
                    }
                    catch { }
                }
                else
                {
                    // 3. Fallback for standalone Desktop App: Launch DWG directly with Windows Shell
                    try
                    {
                        Process.Start(new ProcessStartInfo(rec.DwgPath) { UseShellExecute = true });
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("Không thể mở file bản vẽ: " + ex.Message, "Lỗi", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }

                // Minimize App to taskbar so AutoCAD is immediately front-and-center
                this.WindowState = FormWindowState.Minimized;
            }

            void DoInsertAction()
            {
                var rec = GetSelectedRecord();
                if (rec == null)
                {
                    MessageBox.Show("Vui lòng chọn một kết quả trong danh sách trước!", "Thông Báo", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    return;
                }

                WriteActionFile("INSERT", rec);

                // Try AutoCAD COM direct insert
                bool comSuccess = false;
                try
                {
                    string[] progIds = new string[] {
                        "AutoCAD.Application.24.2",
                        "AutoCAD.Application.24.1",
                        "AutoCAD.Application.24",
                        "AutoCAD.Application"
                    };
                    object acadObj = null;
                    foreach (var pid in progIds)
                    {
                        try { acadObj = Marshal.GetActiveObject(pid); if (acadObj != null) break; } catch { }
                    }
                    if (acadObj != null)
                    {
                        dynamic acad = acadObj;
                        acad.Visible = true;
                        dynamic activeDoc = acad.ActiveDocument;
                        if (activeDoc != null)
                        {
                            activeDoc.Activate();
                            activeDoc.SendCommand(string.Format(CultureInfo.InvariantCulture,
                                "_.-INSERT \"{0}\" 0,0,0 1 1 0 _.EXPLODE (entlast) _.ZOOM _E \n", rec.DwgPath));
                            comSuccess = true;
                        }
                    }
                }
                catch { }

                if (comSuccess)
                {
                    try
                    {
                        var acadProcs = Process.GetProcessesByName("acad");
                        if (acadProcs.Length > 0 && acadProcs[0].MainWindowHandle != IntPtr.Zero)
                        {
                            ShowWindow(acadProcs[0].MainWindowHandle, 9);
                            SetForegroundWindow(acadProcs[0].MainWindowHandle);
                        }
                    }
                    catch { }
                }

                this.WindowState = FormWindowState.Minimized;
            }

            void ExportExcel()
            {
                try
                {
                    string csvPath = Path.Combine(WorkDir, "KET_QUA_TIM_KIEM_THU_MUC.csv");
                    using (var csw = new StreamWriter(csvPath, false, Utf8Bom))
                    {
                        csw.WriteLine("STT,TenToBanDo,ThuaLoai,ChuSuDungDiaChi,SoGcnNgayKy,Layer,Loai,ToaDo_X,ToaDo_Y,DuongDanDayDu");
                        var listToExport = visibleResults.Count > 0 ? visibleResults : allResults;
                        int idx = 1;
                        foreach (var r in listToExport)
                        {
                            string safeThua = "\"" + (r.ThuaLoai ?? "").Replace("\"", "\"\"") + "\"";
                            string safeChu = "\"" + (r.ChuSuDungDiaChi ?? "").Replace("\"", "\"\"") + "\"";
                            string safeGcn = "\"" + (r.SoGcnNgayKy ?? "").Replace("\"", "\"\"") + "\"";
                            string safePath = "\"" + (r.DwgPath ?? "").Replace("\"", "\"\"") + "\"";
                            csw.WriteLine(string.Format("{0},\"{1}\",{2},{3},{4},\"{5}\",\"{6}\",{7},{8},{9}",
                                idx++, r.DwgName, safeThua, safeChu, safeGcn, r.Layer, r.EntType, r.XStr, r.YStr, safePath));
                        }
                    }
                    Process.Start(csvPath);
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Không thể xuất file Excel: " + ex.Message, "Lỗi", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

            void OpenReport()
            {
                try
                {
                    string reportPath = Path.Combine(WorkDir, "BAO_CAO_TIM_KIEM_THU_MUC.txt");
                    if (File.Exists(reportPath))
                    {
                        Process.Start("notepad.exe", reportPath);
                    }
                    else
                    {
                        MessageBox.Show("Chưa có file báo cáo tìm kiếm!", "Thông Báo", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Không thể mở file báo cáo: " + ex.Message, "Lỗi", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

            void ResetForm()
            {
                txtKeyword.Clear();
                txtSubFilter.Clear();
                rbAnd.Checked = true;
                allResults.Clear();
                visibleResults.Clear();
                lvResults.Items.Clear();
                lblResultCount.Text = "✅ Sẵn sàng tìm kiếm";
                lblResultCount.ForeColor = Color.FromArgb(25, 135, 84);
                ResetDetailsPanel();
                txtKeyword.Focus();
            }

            public static void RunGui(string initialFolder, string initialKeyword)
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new FastFindForm(initialFolder, initialKeyword));
            }
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool AttachConsole(int dwProcessId);
        const int ATTACH_PARENT_PROCESS = -1;

        [STAThread]
        static int Main(string[] args)
        {
            try
            {
                if (args.Length == 0)
                {
                    FastFindForm.RunGui("", "");
                    return 0;
                }

                string cmd = args[0].ToLowerInvariant();
                if (cmd == "gui")
                {
                    string fld = args.Length > 1 ? args[1] : "";
                    string kw = args.Length > 2 ? args[2] : "";
                    FastFindForm.RunGui(fld, kw);
                    return 0;
                }

                // If run from a command prompt, attach output to console
                AttachConsole(ATTACH_PARENT_PROCESS);

                if (cmd == "check" && args.Length >= 3)
                {
                    bool force = (args.Length >= 4 && args[3] == "1");
                    DoCheck(args[1], args[2], force);
                    return 0;
                }
                else if (cmd == "check-update")
                {
                    CheckUpdateSilent();
                    return 0;
                }
                else if (cmd == "update-now")
                {
                    bool ok = PerformDirectUpdate();
                    return ok ? 0 : 1;
                }
                else if (cmd == "ingest" && args.Length >= 2)
                {
                    DoIngest(args[1]);
                    return 0;
                }
                else if (cmd == "redecode")
                {
                    DoRedecode();
                    return 0;
                }
                else if (cmd == "search" && args.Length >= 7)
                {
                    int maxRes = 1000;
                    int.TryParse(args[5], out maxRes);
                    if (maxRes <= 0) maxRes = 1000;
                    DoSearch(args[1], args[2], args[3], args[4], maxRes, args[6]);
                    return 0;
                }
                else
                {
                    FastFindForm.RunGui(args[0], args.Length > 1 ? args[1] : "");
                    return 0;
                }
            }
            catch (Exception ex)
            {
                try
                {
                    File.WriteAllText(Path.Combine(WorkDir, "fast_err.log"), ex.ToString());
                }
                catch { }
                return 2;
            }
        }
    }
}
