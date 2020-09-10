<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="創用 CC 授權條款" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">平等權中政治上無力的概念、理論與測量</span>論文與相關程式碼由<a xmlns:cc="http://creativecommons.org/ns#" href="https://doi.org/10.6342/NTU202003889" property="cc:attributionName" rel="cc:attributionURL">Ting-Jhen Jiang</a>創作，以<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">創用CC 姓名標示-非商業性 4.0 國際 授權條款</a>釋出。

# Introduction

這裡包含了所有「江廷振（2020）。〈平等權中政治上無力的概念、理論與測量〉（碩士論文）。國立臺灣大學法律學系，台北市。https://doi.org/10.6342/NTU202003889 」論文中的更正與補充，以及論文中使用的原始碼以及資源。若上述doi網址顯示無資料（因為學校尚未建檔完成關係），[可先連結至此](https://1drv.ms/b/s!AjdlodN9seQ5gvgCcNlBicptM1SrRg?e=UsVWOT)。
原始碼以上述CC條款釋出，歡迎利用，但請依據CC條款標示姓名並且不得用以商業利用。違反者就全部所得利益數額有賠償義務。

# 論文更正與補充

*   頁VII：Abstract：and summarizes the assumption<ins>s</ins> related to political~~ly~~ powerlessness
*   頁XII圖目錄：圖五-~~3~~ <ins>1</ins>：多層次~~關係研究問題~~<ins>資料總體迴歸、分層迴歸、隨機截距與斜率的不同</ins>示意圖
*   頁XII圖目錄：圖五-~~3~~<ins>2</ins>：<ins>追蹤資料的</ins>多層次~~關係研究問題~~<ins>結構</ins>示意圖
*   頁XII圖目錄：圖五-3：多層次~~資料總體迴歸、分層迴歸、隨機截距與斜率的不同~~關係<ins>研究問題</ins>示意圖
*   頁55第1段：可能存在／不存在~~的二個~~ <ins>等</ins>捷思。
*   頁12註解50：各種見解請參照下述章節~~錯誤! 找不到參照來源~~ <ins>2.2.2、3.1</ins>部分。
*   頁96註解280：請見各資料如註 ~~錯誤! 尚未定義書籤。~~ 244、245所示的資料使用說明及報告書」
*   頁105：圖五-~~3~~<ins>1</ins>：多層次~~關係研究問題~~<ins>資料總體迴歸、分層迴歸、隨機截距與斜率的不同</ins>示意圖
*   頁105：圖五-~~3~~<ins>2</ins>：<ins>追蹤資料的</ins>多層次~~關係研究問題~~<ins>結構</ins>示意圖
*   頁105：圖五-3：多層次~~資料總體迴歸、分層迴歸、隨機截距與斜率的不同~~關係<ins>研究問題</ins>示意圖
*   頁117：以及立法委員回應受訪者意見的資料（以下稱回應性資料，~~第一個填補資料集結果~~ <ins>六個多重填補資料集合併結果</ins>）
*   頁137 表六-7：選區立法委員回應性的回應性分析結果<ins>（六個多重填補模型合併結果）</ins>
*   頁156 ：黃昭元 （著）（2017）。〈從平等理論的演進檢討實質平等觀在憲法適用上的難題〉。收於李建良（主編），《憲法解釋之理論與實務》<ins>[Evolution of Equality Theory and Difficulties Concerning the Constitutional Application of the Conception of Substantive Equality]</ins>，第九冊，頁 271-312。 臺北市： 中央研究院法律學研究所。 取自 http://publication.iias.sinica.edu.tw/72217161.pdf 。

# 原始碼與資源

*   01_fetch_ly_meeting_record.R 此檔案中程式碼可用來爬下立法院議事錄HTML檔案。
*   02_fetch_ly_decision_and_votes.R 此檔案中程式碼可用來解析結構化立法委員在每個議案的投票紀錄。
*   02_fetch_ly_decision_and_votes_term56older.R 此檔案中程式碼可用來解析結構化較早屆次與會期中立法委員在每個議案的投票紀錄，涵蓋範圍自 立法院第5屆第5會期第1次臨時會第1次會議紀錄 至 立法院第6屆第3會期第10次會議紀錄。
