---
title: "Research Design and Results"
author:
- name: 江廷振
  affiliation: 國立臺灣大學法律學系碩士班
output:
  html_document:
    keep_md: yes
    toc: true
    toc_depth: 2
    toc_float:
      collapsed: false
      smooth_scroll: false
  word_document: default
  github_document: default
---




```r
Sys.setlocale(category = "LC_ALL", locale = "cht")
```

```
## [1] "LC_COLLATE=Chinese (Traditional)_Taiwan.950;LC_CTYPE=Chinese (Traditional)_Taiwan.950;LC_MONETARY=Chinese (Traditional)_Taiwan.950;LC_NUMERIC=C;LC_TIME=Chinese (Traditional)_Taiwan.950"
```

# 研究設計

## 資料來源與處理

本研究的研究對象聚焦於立法院的政策決定及立法委員的投票表決行為「回應誰的民意」，並且將民意與回應二者間的對應範圍限縮至問卷調查做成後一年半至二年內的立法委員投票表決行為以及國會透過投票通過的決議。

首先，我透過向SRDA學術調查研究資料庫<https://srda.sinica.edu.tw/>申請臺灣社會變遷基本調查「2004第四期第五次公民權組」、「2010第六期第一次：環境組」、「2010第六期第一次：綜合組」、「2016第七期第二次：公民與國家組」的限制版資料，限制版的資料與一般版不同處在於包含有受訪對象的戶籍地村里層級資料，藉此可以準確地與選區資料合併。這幾個資料集中均包含受訪對象對政策的偏好與看法可作為民意資料來源。同時我將各問卷中重複、相同的問題以「2016第七期第二次：公民與國家組」的過錄編碼簿標準合併。研究上選擇這四組調查樣本，第一是因為這四組資料均同時含有基本資料（人口變項）、政策議題調查以及政治參與的資料，第二是因為這四組資料的政策意向問題較多，涵蓋層面較廣而全面（2010年兩組樣本的政策意向問題合併後共有101題、2016公民與國家組的政策意向約共有76題），能夠貼近人們的政策偏好不僅限單一面向或群體利益導向政策的情形，同時對於研究工作而言也能夠在一次的議案編碼後對應比較多問卷調查題目而比較有效率，研究發現與結論有較高的效度。

另外，在立法委員的投票表決行為如何回應民意的資料上，也採取「控制選區」的作法，也就是各個受訪者的意見只與其所處選區的立法委員表決資料串連。控制選區的重要性在於不同的選區會有不同的議題導向與利益需求，例如農業縣與都會商業區可能選民意向以及利益就截然不同。為了排除此項因素的干擾，更聚焦於選民對於立法委員的影響，此處會對於選區控制區別。

研究要驗證的假設，包含：

* 菁英觀點，接近Ely一派代表性補強論的角度，其假說認為立法委員在敘述代表（相似）性上與選民越不相似時，因為偏見或歧視或觀點不一致，因而越不回應選民的需求，敘述代表性越低者越弱勢。依照此理論，用以驗證的虛無假設為：不論選民與立法委員之間的敘述代表相似性如何，立法委員回應選民的程度一致。
* 民主政治的多元主義論，Dahl、Ackerman一派，其假說認為選民可以動員影響立法委員，動員越強或參與政治程度越高時，立法委員受選民壓力越高越回應選民，影響力越低者（通常可能是參與政治程度越低者）越弱勢。根據此理論，用以驗證的虛無假設為：選民不論其參與政治程度為何，立法委員回應的程度一致。

也可以透過與不分區立委（政黨紀律較高因而partisanship較高）與區域立委回應選民的程度對照，若區域立委回應程度較高，也顯示多元主義論較優，菁英論較劣。

除了驗證理論外，此處並同時探索影響回應性的因素。例如探討在請願或抗議的人裡面，是不是又有哪些人影響力較高？

## 變項與模型的建立

首先利用立法院議事暨公報管理系統 <https://lci.ly.gov.tw/LyLCEW/lcivAgendarecMore.action> 檢索立法院院會的議事錄網址，以R軟體爬蟲取得各個議事錄，接著將議事錄文件結尾處的記名表決結果名單內容結構化處理，並透過比對議事錄內容中記名表決關鍵字（以正規表達式搜尋）以及事後檢查檢核並修正，形成一個包含有立法院屆次、會期、會議次、臨時會次、會議時間、投票表決議案編號、立法委員姓名與投票決定（包含贊成、反對、棄權、未出席、未投票）的立法院表決紀錄資料集^[原本研究要利用g0v的「立委投票指南網站」（<https://vote.ly.g0v.tw/>）的資料集，但後來發現有錯誤，於是僅部分參考其公開的程式碼中的演算法與靈感，並修正其錯誤]。

接著與立法院開放資料服務平台提供的歷屆委員資料 <http://data.ly.gov.tw/listcatelog.action?catCd=2> －－包含屆別、姓名、性別、黨籍、黨團、選區名稱、學經歷－－等屬性的資料集以及中央選舉委員會的選舉資料庫資料集<https://data.gov.tw/dataset/13119>、選舉資料庫<http://db.cec.gov.tw/histCand.jsp>、歷史選舉公報<http://bulletin.cec.gov.tw/bin/home.php>串連以增加立委個人基本資料的欄位。此處並建立以下變項（以下部分只要是類別變項，均為虛擬變項，作者使用的R語言分析時能夠自動轉換為虛擬變項）：

* 立法委員敘述代表性：以下列相關變數作為操作指標
    + 立法委員性別：根據中選會資料以及立法院的立法委員個人資料判斷編碼。
    + 立法委員性別與選民性別是否一致：一致編碼為1，不一致編碼為0，為測量敘述代表性的類別變項。
    + 立法委員受教育年數：根據中選會資料以及立法院的立法委員個人資料判斷，並且依照學位等級進行編碼，以國中9、高中/高職12、五專/二專/三專/學士14、技術學院/大學/學士16、研究所/碩士19、博士23編碼。
    + 立法委員受教育年數與選民受教育年數差距：以前述建立的立法委員受教育年數，減去選民的受教育年數後，取絕對值。為測量敘述代表性的連續變項。
    + 立法委員院外／從政前職業社經地位：除去立法委員身份，依據立法委員主要的院外職業或是擔任立法委員前主要的經歷與職業，參考{黃毅志, 2008 #12731}編碼得出連續變項。盡量蒐集並依據最早期從政參選時的選舉公報資料為主。若僅有黨職、聯誼性社團經歷而無其他經歷時，則定位其職業為民意代表，大部分有此類特徵者在選舉公報上面的職業都記載社會服務或是政治工作，地方派系勢力、年輕從政的政二代也多是此模式；如果是從政治人物幕僚開始一路往上爬的政治人物，未經過國會助理（不包含國會辦公室主任此一管理職）或是智庫研究員歷練的也編入民意代表。
    + 立法委員職業社經地位與選民職業社經地位差距：以前述建立的立法委員院外社經地位，減去選民的社經地位後，取絕對值。為測量敘述代表性的連續變項。
    + 立法委員所屬族群：依據資料來源為選舉公報、新聞、Google網頁搜尋所得資料進行編碼，客家政治人物資料來源並參考{何來美, 2017 #12749}，分為台灣閩南人、台灣客家人、大陸各省市、台灣原住民、外裔或原國籍為外籍或原國籍中國大陸的新移民、前述分類以外的臺灣人。
    + 立法委員族群與選民族群是否一致：依據先前所建立的立法委員族群以及選民族群變項進行判斷，一致編碼為1，不一致編碼為0，為測量敘述代表性的類別變項。
    + 立法委員與選民年齡差距：以立法委員的年齡減去選民的年齡後，取絕對值。為測量敘述代表性的連續變項。
    + ~~立法委員與選民敘述代表性整體差距：因素分析or試題反應理論萃取~~


在民意調查資料中，將整個民意資料集以各個政策意向的問題為對照基礎，將資料從短資料轉換為長資料－－也就是一個觀察值從代表一個受訪者的全部變數，轉換為一位民眾就一個政策議題表達意向的全部變數－－並且選擇以下幾個與本研究有關的項目並處理編碼：

* 議題領域：分為經濟、公民與政治權、社會福利、財政、內政、兩岸、環境、經濟社會文化權。
* 同政策意向者佔全國／全選區整體意向比率：分別以全國（全調查樣本）／全選區（全調查樣本中同一選區的受訪對象）層級中與觀察對象持同方向政策意向者人數除以全部人數所得數值編碼。Likert五點或四點量表設計的問題中，將回答非常贊成與贊成某政策者編為同一類，非常反對／反對某政策者編為同一類。這個指標也就是要衡量觀察對象的政策意向連結整體層級的民意強度。
* 意向強度：如果題目問題設計以Likert五點或四點量表設計的問題中，贊成、反對等選項編碼為1，非常贊成、非常反對等選項編碼為2。若題目設計僅有三等（贊成、無意見、反對）或強迫回答的二等時，均編碼為1，其餘無意見編碼為0，為序數尺度變項。
* 民眾認為重要議題：2004公民組的「今後十年的國家目標中,您認為哪個最重要?」、「今後十年的國家目標中,您認為哪一個次要的?」（答案：   高度的經濟成長、保護本國有強大的國防力量、看到人們在他們的工作中有更[多]的發言權、盡量使我們的都市和鄉村更美麗）；2010環境組的「請問您認為以下哪個項目是目前台灣社會最重要的議題?」、「哪一項議題是目前台灣社會第二重要的?」（答案：健康照顧、教育、犯罪、環境、移民、經濟、恐怖主義、貧窮、以上皆非）；
* 族群：四個問卷的問題皆有共同的「父母親是哪裡人」的兩個選擇題及開放填充題，我將此題目依據答案重新編碼為父母親分別為台灣閩南人、台灣客家人、大陸各省市、台灣原住民、外裔或原國籍為外籍或原國籍中國大陸的新移民、前述分類以外的臺灣人（例如有部分受訪者的父母親是「外省人第二代」，而在開放填充題中回答臺灣人）。接著依照廣義的原生論（primordialism），只要父母有其一為人口比例較少的較少數族群者，則一律視為較少數族群。為類別變項。
* 所屬族群人口比例：將前述的族群類別變項，依據各族群佔全國人口比例編碼為數值的連續變項。人口比例的資料來自於行政院客家委員會委託研究的「99年至100年全國客家人口基礎資料調查研究」以及「105年度全國客家人口暨語言調查研究報告」<https://www.hakka.gov.tw/Content/Content?NodeID=626&PageID=37585>、內政部戶政司人口資料庫<https://www.ris.gov.tw/zh_TW/346>、內政部移民署業務統計資料<https://www.immigration.gov.tw/lp.asp?ctNode=29699&CtUnit=16434&BaseDSD=7&mp=1>。
* 教育程度：以受訪者填答的教育程度，綜合其學位等級轉換為受教育年數為準的數值編碼為受教育年數，轉換標準為：無/不識字/識字/私塾/自修0^[外某受訪者回覆日本教育讀三年，經比對檢視其他回覆內容後以0編碼]
；小學6；國(初)中、初職 9 ^[某受訪者回覆空軍子弟小學的師範學校，經檢視其他回覆內容後以9編碼]；高中、綜合高中、高職、高中職業科、高中普通科、中正預校 12 ^[某受訪者回覆宜蘭特教學校，比對該受訪者對於「請問您從國小到現在,總共受幾年的學校教育?」題目反應為12，並經查證宜蘭的特教學校有高中職學制，因此編碼為12]；軍警校專修班、軍警專修班 13；士官學校、五專、二專、二專、三專 14；軍警校專科班、軍警專科班、空中行(商)專；空中大學、軍警官學校/大學、軍警官校或大學、技術學院、科技大學、二技、四技、大學 16 ^[某受訪者回覆基督學院，經查證後該校授與學士學位，同樣編碼為16]；碩士 19；博士 23。
* 家庭收入：主要依據為問卷問題為「包括各種收入來源,您全家人的所有收入,每個月大約多少元?」，原調查得到的資料為根據每組不同所得範圍的區間組別，此處重新根據各組組中點的編碼為收入，最低一組（無收入）為0，最高一組（100萬元以上）編碼為150,000。
* 職業社經地位：以各問卷中的「（退休前或未退休）工作主要的職位和工作內容是?變遷職位碼」問題參考{黃毅志, 2008 #12731}轉換得出一職業社經地位的量化連續尺度變項。
* 綜合社經地位／階級：以因素分析法從前述教育程度、個人收入（某問卷題項為工作收入，此處同樣以個人收入定義）、家庭收入及職業社經地位等變項，以因素分析法萃取出共同因素（主軸因素），過程運用由R語言中的factanal函數，該函數以最大概似法進行參數估計，並設定以最大變異法(Varimax)轉軸，經Bartlett檢定均適合以一個共同因素進行因素分析。以下為各因素負荷量及變異解釋程度：
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> 2004公民綜合社經地位因素負荷量 </th>
   <th style="text-align:right;"> 2010環境綜合社經地位因素負荷量 </th>
   <th style="text-align:right;"> 2010綜合綜合社經地位因素負荷量 </th>
   <th style="text-align:right;"> 2016公民綜合社經地位因素負荷量 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 教育程度 </td>
   <td style="text-align:right;"> 0.675 </td>
   <td style="text-align:right;"> 0.668 </td>
   <td style="text-align:right;"> 0.639 </td>
   <td style="text-align:right;"> 0.697 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 職業社經地位 </td>
   <td style="text-align:right;"> 0.738 </td>
   <td style="text-align:right;"> 0.700 </td>
   <td style="text-align:right;"> 0.621 </td>
   <td style="text-align:right;"> 0.763 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 個人收入 </td>
   <td style="text-align:right;"> 0.564 </td>
   <td style="text-align:right;"> 0.598 </td>
   <td style="text-align:right;"> 0.510 </td>
   <td style="text-align:right;"> 0.541 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 家庭收入 </td>
   <td style="text-align:right;"> 0.420 </td>
   <td style="text-align:right;"> 0.567 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 0.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 可解釋變異比例 </td>
   <td style="text-align:right;"> 0.374 </td>
   <td style="text-align:right;"> 0.403 </td>
   <td style="text-align:right;"> 0.295 </td>
   <td style="text-align:right;"> 0.374 </td>
  </tr>
</tbody>
</table>
* ~~工作狀態：略~~
* 政治效能感：分為外在效能感（個人感覺政府有去回應個人需求的能力）與內在效能感（個人感覺自己有去瞭解及參與政治的能力），依據問卷不同的題目作為政治效能感的指標，如指標包含不只一題，則以各題分數加總之後平均計算。操作方式如下表：
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">外在效能感</div></th></tr>
  <tr>
   <th style="text-align:left;"> 2004公民 </th>
   <th style="text-align:left;"> 2010環境 </th>
   <th style="text-align:left;"> 2010綜合 </th>
   <th style="text-align:left;"> 2016公民 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> v47 像我這樣的人,對政府的作為沒有任何影響力 </td>
   <td style="text-align:left;"> v61 請問您覺得政府有沒有能力解決台灣的環境污染問題? </td>
   <td style="text-align:left;"> v67d請問您贊不贊成一般公民也可以影響政府的決策的說法? </td>
   <td style="text-align:left;"> d16a 像您這樣的人,對政府的作為沒有任何影響力 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v48 像我這樣人,我不認為政府在乎我的想法是什麼 </td>
   <td style="text-align:left;"> v70a 請問您同不同意以下的說法?在台灣,一般民眾影響環保政策的機會非常有限 </td>
   <td style="text-align:left;"> v67h 請問您贊不贊成只要經常提出意見,像我們這樣的人也能影響社會的發展的說法? </td>
   <td style="text-align:left;"> d16b 一般公民對政治都有相當的影響力 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v52 如果您或你們採取行動的話,那麼【立法院】會不會慎重考慮您或你們的要求? </td>
   <td style="text-align:left;"> v78 請問您覺得自己平日的環保行為,對於改善台灣的環境品質有沒有用? </td>
   <td style="text-align:left;"> v67i 請問您贊不贊成只要大家努力去做,社會與政治的改進並不是難事的說法? </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">內在效能感</div></th></tr>
  <tr>
   <th style="text-align:left;"> 2004公民 </th>
   <th style="text-align:left;"> 2010環境 </th>
   <th style="text-align:left;"> 2010綜合 </th>
   <th style="text-align:left;"> 2016公民 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> v49 對台灣所面臨的一些重大政治問題,我覺得自己還頗為了解 </td>
   <td style="text-align:left;"> v21a 請問您認為自己對於(20a、20b)發生的原因瞭不瞭解?(1一點也不瞭解,5非常瞭解) </td>
   <td style="text-align:left;"> v67f 請問您贊不贊成公眾的事不好辦,所以最好不要插手的說法? </td>
   <td style="text-align:left;"> d16c 您覺得您對於國家重要的政治議題,都有相當的瞭解 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v50 我認為台灣大部分的人,都比我更知道政治與政府的事 </td>
   <td style="text-align:left;"> v21b 對於(20a、20b)的解決方式,請問您認為自己瞭不瞭解?(1一點也不瞭解,5非常瞭解) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> d16d 您認為大多數人都比您更瞭解政治與政府 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v51 如果有這種情形發生的話,您可不可能自己一個人或者和其他人一起去採取行動? </td>
   <td style="text-align:left;"> v26a 請問您同不同意以下說法?對您而言,要為環境做些事太困難 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v26b 請問您同不同意以下說法?就算要多花些金錢或時間,您也願意為環境做些正確的事 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v26c 請問您同不同意以下說法?生活中有比保護環境更重要的事 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v26d 請問您同不同意以下說法?除非大家都一起為環境盡力,否則您一個人做沒有用 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v79 對您來說,您覺得日常生活中的環保行為容不容易做到? </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
* ~~相對剝奪感：以受訪者對對現狀的評價為依據，主要題目為還沒編。「您覺得政府現行的環境保護政策符不符合公平正義的原則?」~~
* 政治興趣：d15 請問您個人對政治有沒有興趣?（2016公民）
* 非競選期間政治參與：依據原本問卷設計中的下述次序變項，以應用試題反應理論的graded response model計算測量出一個潛在連續型態的潛在變數：
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> 2004公民 </th>
   <th style="text-align:left;"> 2010環境 </th>
   <th style="text-align:left;"> 2010綜合 </th>
   <th style="text-align:left;"> 2016公民 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> v28 您有沒有做過請願(簽名)連署 </td>
   <td style="text-align:left;"> v34 請問您是環保團體的成員嗎? </td>
   <td style="text-align:left;"> v79a 在過去的一年,您有沒有向政府官員,民意代表或政黨反映意見提出要求? </td>
   <td style="text-align:left;"> h2a 您過去有沒有做過或將來會不會做這些事?a 請願(簽名)連署 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v29 您有沒有因為政治,倫理道德,或是環保的理由拒絕購買或是特別去購買某些產品 </td>
   <td style="text-align:left;"> v35a 在過去五年間,請問您有沒有做過以下的事情:連署一份有關環保議題的請願書 </td>
   <td style="text-align:left;"> v79b 在過去的一年,您有沒有向大眾媒體投訴? </td>
   <td style="text-align:left;"> h2b 您過去有沒有做過或將來會不會做這些事?b 因為政治的、倫理(道德)的、或是環保的理由拒絕購買或是特別去購買某些產品 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v30 您有沒有參加示威遊行 </td>
   <td style="text-align:left;"> v35b 在過去五年間,請問您有沒有做過以下的事情:捐款給環保團體 </td>
   <td style="text-align:left;"> v79c 在過去的一年,您有沒有透過網際網路反映意見? </td>
   <td style="text-align:left;"> h2c 您過去有沒有做過或將來會不會做這些事?c 參加示威遊行 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v31 您有沒有參加政治集會或造勢活動 </td>
   <td style="text-align:left;"> v35c 在過去五年間,請問您有沒有做過以下的事情:參加有關環保議題的抗議行動或遊行 </td>
   <td style="text-align:left;"> v79d 在過去的一年,您有沒有參加遊行,示威,靜坐或其他自力救濟方式? </td>
   <td style="text-align:left;"> h2d 您過去有沒有做過或將來會不會做這些事?d 參加政治集會或造勢活動 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v32 您有沒有找過政治人物或公務人員表達您的看法 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2e 您過去有沒有做過或將來會不會做這些事?e 找政治人物或公職人員表達您的看法 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v33 您有沒有捐錢給某個社會或政治活動,或者幫他們募款 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2f 您過去有沒有做過或將來會不會做這些事?f 捐錢給某個社會或政治活動,或者幫他們募款 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v34 您有沒有透過媒體去表達您的看法 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2g 您過去有沒有做過或將來會不會做這些事?g 透過媒體去表達您的看法 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v35 您有沒有參加網路上的政治論壇或討論群組 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2h 您過去有沒有做過或將來會不會做這些事?h 透過網路表達您的政治想法 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v36 請問您有沒有加入和參與政黨 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3a 您過去有沒有做過或將來會不會這樣做?a 跟親朋好友討論不公不義的事情或現象 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v37 請問您有沒有加入和參與工會,工商同業公會,職業同業公會 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3b 您過去有沒有做過或將來會不會這樣做?b 參加抗議不公不義的活動 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v38 請問您有沒有加入和參與宗教團體或教會 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3c 您過去有沒有做過或將來會不會這樣做?c 以金錢來支持弱者或公義團體 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v39 請問您有沒有加入和參與運動,休閒或文化團體 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v40 請問您有沒有加入和參與其他自願性社團 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
    + 是否投票：包含2010綜合的「前年立法委員選舉(民國97年1月)的時候,請問您有沒有去投票?」、2010環境的「請問您上一次總統選舉(97年3月)有沒有去投票?」、2016公民的「在這一次(一月十六日)舉行的總統大選中,請問您有沒有去投票?」，選項編碼為有、無、沒有投票權、遺漏值，為類別變項。
    + 是否曾經透過接觸政治人物如民意代表或政府官員提出訴求，或者連署及請願：包含2010綜合的「在過去的一年,您有沒有向政府官員,民意代表或政黨反映意見提出要求?」（選項為多次、曾有過、從未有過、遺漏值）、2010環境的「在過去五年間,請問您有沒有做過以下的事情:連署一份有關環保議題的請願書」（選項為有、沒有、遺漏值）、2016公民的「您過去有沒有做過或將來會不會做這些事?a請願(簽名)連署／e找政治人物或公職人員表達您的看法」（選項為有做過：過去一年中您有做過這件事；有做過：在更早以前您有做過這件事；沒有做過：就算過去沒有做過,將來您有可能做這件事；沒有做過：過去沒有做過,而將來無論在什麼情形下您也不會做這件事；遺漏值）。重新定義選項並編碼有做過、沒做過、遺漏值；2016公民的兩個題目合併，只要其中任一項有做過就編碼為有做過。為類別變項。
    + 是否曾經參與集會遊行抗議：包含2010綜合「在過去的一年,您有沒有參加遊行,示威,靜坐或其他自力救濟方式?」、2010環境「在過去五年間,請問您有沒有做過以下的事情:參加有關環保議題的抗議行動或遊行」、2016公民的「您過去有沒有做過或將來會不會做這些事?c參加示威遊行／b參加抗議不公不義的活動」。選項的尺度均與前面連署及請願、接觸政治人物的原始問卷選項相同。此處重新定義選項並編碼為有參加過、沒參加過、遺漏值。為類別變項。
* 統獨傾向：潛在類別模式
* 政黨傾向：潛在類別模式

隨後我以表決紀錄／表決議案的資料集，在此資料集中建立與議案與民意之間的關係以及議案屬性。在建立完成關連之後，透過SPSS軟體將資料檔輸出過錄編碼簿（也就是每一個問題有哪些答案的資料集），接著將此過錄編碼簿串連前述表決議案資料集，並且針對每一個議案與答案選項的關連編碼出「議案的立場」。特別需要說明的是某些表決議案涉及多方向利益角力的編碼方式，舉兩個代表案例說明，首先是立法院第7屆第6會期第14次會議第24表決議案<https://lci.ly.gov.tw/LyLCEW/html/agendarec/02/07/06/14/LCEWC03_070614.htm>，涉及的是當時二代健保的補充保費修正案，表決結果決定新增補充保費，費用來源為獎金、兼職薪資所得、執行業務收入、股利所得、利息所得、租金收入（被稱為林志玲條款）。相對應的題目是2010綜合問卷的「您是不是贊成如果大家的收入更平均的話,一般人會因此更不努力工作的說法?」（回答：1非常贊成;2贊成;3不贊成;4非常不贊成;……其他遺漏值）以及「有人說:減少高收入與低收入之間的差距,是政府的責任,請問您同不同意?」（回答：1非常同意;2同意;3無所謂同不同意;4不同意;5非常不同意;……其他遺漏值）。補充保費的決定實施看似比原先舊制更往所得重分配的方向走，但如果檢視立法過程中的對案可以發現反對所得重分配的利益並沒有全輸，因為反對陣營的對案中的「退職金、海外所得、買賣資產所得」並未被納入，在此種情形下，我的編碼方式是將此議案與兩個問題建立兩次的關連，接著在「議案的立場」方面，第一次的關連編碼回應了贊成所得重分配者的意見，第二次的關連編碼回應了反對所得重分配者的意見。同樣的情形發生在立法院第9屆第2會期第13次會議第2表決議案<https://lci.ly.gov.tw/LyLCEW/html/agendarec1/02/09/02/13/LCEWC03_090213.htm>，涉及的是勞基法一例一休修法中以休息日加班要發加倍加班費的改變，相關連的題目是2016公民的「請問您是贊成還是反對?f透過減少每個人的工作時數,讓更多的人可以工作」（回答1很贊成;2贊成;3既不贊成也不反對;4反對;5很反對;……其他遺漏值）及「應不應該是政府的責任?e為工商業成長所需提供協助」（回答1當然應該;2還算應該;3不太應該;4當然不應該;……其他遺漏值），議案本身表面上看起來與舊制相比回應了贊成減少工時者以及認為政府不應該透過減少工時協助工商業成長者的意見，但檢視對案可以看到這方面的利益也並未完全勝利，因為「二例假日」的訴求並未成功，在此情形下也一樣建立兩次關連，而有關「議案的立場」則分別給予相反的編碼。

在上述資料集處理完成後分別將各個資料集串聯中央選舉委員會在政府資料開放平台釋出的選舉資料庫<https://data.gov.tw/dataset/13119>（並且修正若干錯誤）後將各個資料集增加選舉屬性資料。接著，選擇各個民意調查做成後一年期間（也就是2010年7月至2011年6月、2016年8月至2017年5月）內的表決紀錄資料，以這段期間的資料為準再將資料串連合併。串聯合併條件分別是將選區選民（受訪者）和該選區立法委員串在一起，以及不分選區將所有立法委員和所有選民串在一起，這也分別就是立法委員代表全國選民以及立法委員代表選區選民的情形，同時也落實前面研究設計所提到的控制選區的問題；串聯方式為inner join，也就是僅留存有共同欄位的值相同的觀察值。總計在此期間內有147個表決議案可以對應到問卷的問題。建立變項如下：

* 是否回應民意（應變項）：對照前述處理資料所建立的「議案的立場」，若選民的立場與議案的立場一致，而立法委員投票贊成時，編碼為3（回應），投票反對時編碼為0（拒絕）；選民的立場與議案的立場相反而立法委員投票反對時，編碼為3（回應），投票贊成時編碼為0（拒絕）；投下棄權票時，編碼為2（棄權）；立法委員未出席會議、出席會議但未投票，編碼為1（忽略）。研究在此先將「忽略」的情形獨立出來，僅於必要以及敘述統計時利用，不納入研究。其餘的「拒絕」、「棄權」以及「回應」，則有明顯的順序關係，此為一有順序（ordinal）關係的類別變項。

控制變項上，則有
* 立法委員團結分數：參考{黃秀端, 2006 #12613}的作法，依據一表決議案中與觀察值立法委員做出相同立場投票（決定）的同黨籍立法委員數，除以該議案中同黨籍立法委員的決定總數的數值連續變項編碼。此變項其實也相當於政黨動員比率／政黨施壓率。分母所謂同黨籍立法委員的決定總數，也把未出席院會、棄權、出席院會但不投票也視為一種決定。

每一個觀察值代表一個意見，共計觀察值數目：


資料檢核後，各變項遺漏值如下：


## 模型與研究假設

由於應變項為次序變項，不同類別之間雖有次序關係但間隔可能不等距，迴歸分析使用ordinal logit models{Agresti, 2010 #12745}

有沒有需要使用Structural  Equation  Model,  SEM或是Multilevel Model, MLM(可能不同選區有不同選區的特性？)



## 信度檢測（還沒做）

Cronbach’s α

## 共線性檢測（還沒做）

共線性檢測：可以用correlation analysis相關係數矩陣、VIF test(超過10 drop)
http://r-statistics.co/Model-Selection-in-R.html （also tutorial on stepwise）

## 預測失準可能檢測

Heteroscedasticity: prediction必須要隨機分布，不能有pattern，否則導致模型失準； spot outliers

## 研究結果與發現

standardized beta coefficient

https://www.princeton.edu/~otorres/LogitR101.pdf
https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
https://www.jakeruss.com/cheatsheets/stargazer/


### 立法委員回應民意的情形會因為執政與在野不同，且不一定受民意多數的影響

首先發現一個值得注意的現象。選民影響力與國會議員行為研究間關係的始祖{Miller, 1963 #12740}發現，民意在一定程度上會影響民意代表的行為。但在本研究發現顯示，在2010年7月至2011年6月（以下簡稱第七屆研究範圍期間）以及2016年8月至2017年5月（以下簡稱第九屆研究範圍期間）兩段期間中，民意是否佔多數對於立法委員是否回應民意而言，影響程度卻是不一定，包含是否顯著影響、效應的方向及大小皆有不同：第七屆研究範圍期間立委的行為與多數民意呈現顯著方向相反的關係，第九屆研究範圍期間立委的行為則是顯著跟著民意多數變化，但效應不高。但是，同樣當立法委員意向越傾向政黨，黨性越強（stronger partisanship）時，越傾向不回應民意。（立法委員回應民意與民意多數、政黨意見多數間關係分析表；report其他統計指標）





```r
##stargazer(model_influce_from_p_p.k.2,model_influce_from_p_p.k.3,model_influce_from_p_p.k.4,model_influce_from_p_p.k.5,model_influce_from_p_p.d.2,model_influce_from_p_p.d.3,model_influce_from_p_p.d.4,model_influce_from_p_p.d.5, title="立法委員回應民意與民意佔比、同黨成員意見佔比間關係分析表", align=TRUE, type = 'html', summary=TRUE, notes="model 1,2,3,4 為第七屆研究範圍期間,model 5,6,7,8 為第九屆研究範圍期間")
```

為了探究這種與一般直覺相左的原因，瞭解民意代表為何會選擇不回應民意，此處先區分時期、區分政黨回應民意的情形，以箱型圖觀察如下：


```r
##rm(model_influce_from_p_p.k.2,model_influce_from_p_p.k.3,model_influce_from_p_p.k.4,model_influce_from_p_p.k.5,model_influce_from_p_p.d.2,model_influce_from_p_p.d.3,model_influce_from_p_p.d.4,model_influce_from_p_p.d.5)
##gcreset()
#glmdata %>%
#  dplyr::filter(!is.na(respondopinion)) %>% ggplot(aes(x=respondopinion, y=opinion_pressure_from_constituent_by_nation)) + labs(title = "第七屆與第九屆研究範圍期間立法委員回應民意與全國民意佔比間關係") + facet_grid(term ~ party) + geom_boxplot()

#glmdata %>%
#  dplyr::filter(!is.na(respondopinion)) %>%
#  ggplot(aes(x=respondopinion, y=opinion_pressure_from_constituent_by_electionarea)) + labs(title = "第七屆與第九屆研究範圍期間立法委員回應民意與立法委員選區多數民意佔比間關係") + facet_grid(term ~ party) + geom_boxplot()
```

從箱型圖中可以發現兩個時期的在野黨均明顯較執政黨更回應民意多數。這一點與{Miller, 1963 #12740}的研究發現指出非現任者會傾向更回應民意一點有相似的現象。
將政黨席次與執政黨席次的差距作為自變項加回迴歸式進行檢定。




```r
##stargazer(model_influce_from_p_p_s.k.2,model_influce_from_p_p_s.k.3,model_influce_from_p_p_s.k.4,model_influce_from_p_p_s.k.5,model_influce_from_p_p_s.d.2,model_influce_from_p_p_s.d.3,model_influce_from_p_p_s.d.4,model_influce_from_p_p_s.d.5, title="立法委員回應民意與民意佔比、政黨成員意見佔比及及所屬政黨與執政黨間席次差距關係分析", align=TRUE, type = 'html', summary=TRUE, notes="model 1,2,3,4 為第七屆研究範圍期間,model 5,6,7,8 為第九屆研究範圍期間")
```

以上分析顯示民意的多寡與強度乃至於人數並不當然能夠影響到民意代表的行為。(reports odds)這種現象也就隱含著影響立法委員的因素還有其他來源，也許包含政黨、金錢、媒體、民意傳達成效或是更有影響力的人民。

### 各種因素對民意代表投票的影響力




```r
#stargazer(model_influce_from_all_s.k.2,model_influce_from_all.d.2, title="立法委員回應民意與各因素關係分析", align=TRUE, type = 'html', summary=TRUE, notes="model 1 為第七屆研究範圍期間,model 2 為第九屆研究範圍期間")
```

### 政治資本與政治參與對於民意代表是否回應的中介效果

中介變項檢驗方法mediational effects
http://data.library.virginia.edu/introduction-to-mediation-analysis/
https://www.jstatsoft.org/article/view/v059i05
檢測
https://gist.github.com/stephlocke/fb1225f6b5029a9f5b04aa6e6123cbc9

## 探索性資料分析


```r
#threelevelglmdata<-mutate_cond(glmdata,respondopinion==1,respondopinion=2) %>%
#  dplyr::filter(respondopinion %in% c(0,2,3)) %>%
#  dplyr::select(term,respondopinion,myown_areakind,myown_sex,myown_age,myown_dad_ethgroup,myown_mom_ethgroup,myown_eduyr,myown_int_pol_efficacy,myown_ext_pol_efficacy,myown_approach_to_politician_or_petition,myown_protest,myown_vote,myown_working_status,myown_ses,myown_family_income_ingroup,myown_family_income,myown_family_income_ranking,myown_family_income_stdev,percent_of_same_votes_from_same_party,rulingparty,opinionstrength,eduyrgap,sesgap,sexgap,agegap,opinion_pressure_from_constituent_by_nation,opinion_pressure_from_constituent_by_electionarea,myown_factoredclass,issue_field1,party) %>%
#  mutate_at("respondopinion",funs(ordered)) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (myown_eduyr)
#           )
#       ) + labs(title = "受教育年") + #facet_grid(term+issue_field1 ~ party) + #geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (myown_ses)
#           )
#       ) + labs(title = "職業社經地位") + #facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (myown_factoredclass)
#           )
#       ) + labs(title = "綜合社經地位") + facet_grid(term+issue_field1 ~ party) + #geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = myown_family_income_ingroup,
#           fill = (respondopinion)
#       )
#) + labs(title = "家庭收入所屬組別") + facet_grid(term ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (myown_family_income)
#           )
#       ) + labs(title = "家庭收入") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (myown_family_income_stdev)
#           )
#       ) + labs(title = "家庭收入多少標準差") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (percent_of_same_votes_from_same_party)
#           )
#       ) + labs(title = "同黨成員同立場比例") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (eduyrgap)
#           )
#       ) + labs(title = "選民與立法委員教育年差距") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (sesgap)
#           )
#       ) + labs(title = "選民與立法委員社經地位差距") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = respondopinion,
#           y = (agegap)
#           )
#       ) + labs(title = "選民與立法委員年齡差距") + facet_grid(term+issue_field1 ~ party) + geom_boxplot()) 

#(ggplot(threelevelglmdata,
#       aes(x = myown_dad_ethgroup,
#           fill = (respondopinion)
#       )
#) + labs(title = "父親族群") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = myown_mom_ethgroup,
#           fill = (respondopinion)
#       )
#) + labs(title = "母親族群") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = myown_approach_to_politician_or_petition,
#           fill = (respondopinion)
#       )
#) + labs(title = "有無請願或找政治人物") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = myown_protest,
#           fill = (respondopinion)
#       )
#) + labs(title = "有無抗議") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = myown_vote,
#           fill = (respondopinion)
#       )
#) + labs(title = "有無投票") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))

#(ggplot(threelevelglmdata,
#       aes(x = sexgap,
#           fill = (respondopinion)
#       )
#) + labs(title = "立法委員和選民性別差異") + facet_grid(term+issue_field1 ~ party) + geom_bar(position="fill"))
```

## 其他模型：隨機森林

https://github.com/thomasp85/lime

## 其他模型：決策樹


```r
#require(rpart)
#require(rpart.plot)
#set.seed(22)
#train.index <- sample(x=1:nrow(threelevelglmdata), size=ceiling(0.8*nrow(threelevelglmdata) ))
#train <- threelevelglmdata[train.index,1:26]
#test <- threelevelglmdata[-train.index,1:26]
#cart.model<- rpart(respondopinion ~ ., 
#                   data=train)
#rattle::fancyRpartPlot(cart.model, cex=1.1,sub="")
```






binary logistic





When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:



## Including Plots

You can also embed plots, for example:




```r
getwd()
```

```
## [1] "E:/Software/scripts/R/vote_record/docs"
```

## sessionInfo()


```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 17763)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Chinese (Traditional)_Taiwan.950 
## [2] LC_CTYPE=Chinese (Traditional)_Taiwan.950   
## [3] LC_MONETARY=Chinese (Traditional)_Taiwan.950
## [4] LC_NUMERIC=C                                
## [5] LC_TIME=Chinese (Traditional)_Taiwan.950    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] kableExtra_0.9.0 dplyr_0.7.8      knitr_1.20      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0        rstudioapi_0.8    xml2_1.2.0       
##  [4] bindr_0.1.1       magrittr_1.5      hms_0.4.2        
##  [7] munsell_0.5.0     rvest_0.3.2       tidyselect_0.2.5 
## [10] viridisLite_0.3.0 colorspace_1.3-2  R6_2.3.0         
## [13] rlang_0.3.0.1     highr_0.7         httr_1.4.0       
## [16] stringr_1.3.1     tools_3.5.2       rowr_1.1.3       
## [19] htmltools_0.3.6   yaml_2.2.0        assertthat_0.2.0 
## [22] digest_0.6.18     tibble_1.4.2      crayon_1.3.4     
## [25] bindrcpp_0.2.2    purrr_0.2.5       readr_1.3.0      
## [28] glue_1.3.0        evaluate_0.12     rmarkdown_1.11   
## [31] stringi_1.2.4     compiler_3.5.2    pillar_1.3.1     
## [34] scales_1.0.0      pkgconfig_2.0.2
```
