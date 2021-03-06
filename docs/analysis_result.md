---
title: "政治上無力者的概念、理論與測量"
author:
- name: 江廷振
  affiliation: 國立臺灣大學法律學系碩士班
date: 'First created on Jan 15, 2019, 09:00. Last updated on 2019-01-16 07:17:10'
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




```
## [1] "LC_COLLATE=Chinese (Traditional)_Taiwan.950;LC_CTYPE=Chinese (Traditional)_Taiwan.950;LC_MONETARY=Chinese (Traditional)_Taiwan.950;LC_NUMERIC=C;LC_TIME=Chinese (Traditional)_Taiwan.950"
```

# 研究背景

## 平等權關注經驗層次的弱勢群體取向

在近幾年全世界的婚姻平權運動中，我們可以看到數個國家中，持反對同性戀婚姻的多數民意與支持同性戀婚姻的司法權立場相反，而這些國家的司法權仍反多數民意做出支持同性戀婚姻的裁判例子。為什麼司法權可以推翻具有民主正當性的國會通過的立法？這個所謂的「反多數決困境」在理論與實際上一直影響著司法運作，也成為憲法學者關心的焦點。對於這個問題，以美國為首發展出了實體價值取向與程序價值取向的兩種反多數決理論。其中程序價值取向的理論，主張由於在某些情形下民主程序會產生「失靈」的現象，為了維護民主制度司法權必須要介入導正，因此司法審查有其正當性。至於什麼情形下構成「失靈」？比較著名的理論主張包含：1.如果法律有限制民主發展與健全或限制政治競爭時（例如在任者為了限制競爭，通過自利型法案限制言論自由與選舉權），為了暢通政治變遷管道，司法權無須退讓；以及2.對於「分散又隔離的少數」（discrete and insular minorities）的偏見，使得通常能夠保障少數的立法程序被阻礙時，此時需要司法者特別審視{Ely, 1980 #9289}。

本研究以程序取向反多數決理論的第二個關懷面向為重心，從「分散又隔離的少數」這個概念出發，融入、結合當代平等權領域發展中實質關心對象──「弱勢」^[平等權之所以會轉向並融入重視弱勢群體的「實質平等」觀念，最重要的發展起源莫過於針對美國過去歷史上種族隔離現象而生的seperate but equal判例予以推翻的Brown v. Board of Education案，透過注意到種族隔離對於黑人的心理傷害而使得平等原則的概念不再僅審視法律上的平等對待，而是去檢視結果、影響、地位等因素。]，並且再從「弱勢」這個構念細緻化至「政治上的無力者」此一法院實務判決曾指出並且在學理上也有學者主張的弱勢面向作為探討主軸（例如結構性不平等的政治體制，所產出的決策就會傾向維護現有的不平等狀態）{Young, 1990 #7979}，試圖將政治上的無力者這個構念進一步透過國會研究的視野進行細緻化操作。

## 司法裁判對於政治上無力者的分析與觀察不足之處

事實上，在實際的司法裁判案例上就已經針對誰是政治上的無力者一點有過爭論與操作，但是操作過程以及觀察研究方法顯得粗糙而需要改進。我先從對於司法裁判上關於誰是政治上無力者的操作方式檢討作為引子。美國的Scalia大法官曾在Romer v. Evans一案中，主張由於財富是影響力的來源，在同性戀有高可支配所得下，他們的政治力量大於他們的人口比例，所以同性戀不是弱勢{, 1996 #12499@645-646}。無獨有偶，我國的[大法官釋字第748號解釋](https://www.judicial.gov.tw/constitutionalcourt/p03_01_1.asp?expno=748)（有關民法規定婚姻僅限同性婚姻是否違反平等權的問題爭議）中，以第8段到第9段的幾個同性婚姻法案在國會所面臨的困境與阻礙（例如祁家威先生屢次請願、訴訟被無視、同性婚姻法案一直卡關）等事實來認定同性戀屬於政治程序上的弱勢。

上述論述中，先不提Scalia大法官的主張只是停留在假說的提出與主張而未進入經驗資料驗證的階段。這些論述在理論、概念操作與測量環節均有缺陷不足之處。例如測量環節的問題共有三點，第一是直接以研究處理（treatment，相當於實驗刺激、自變項，在Scalia大法官的主張是「財富」，在748號解釋中則為「人口結構」、「人數」、「刻板印象歧視」等因素）連結後測──即觀察同性戀者參與並動員起來透過社會運動爭取認同／在相關請願及法案審議都失敗的結果──得出實驗結論：同性戀是政治上的強勢者／「[同性戀]……因人口結構因素，為社會上孤立隔絕之少數，並因受刻板印象之影響，久為政治上之弱勢」（自變項與應變項間呈現顯著關連），就此而言缺乏前測對照以估計實驗組的研究處理前後組內差異。

第二個測量問題是缺乏利用控制組──也就是在實驗中因為隨機分派受試者接受處理，所以除了未接受處理外其他所有方面都和實驗組相同或類似的一群人，在此例中相當於人口數較大的群體──與實驗組的組間差異對照以瞭解真實處理效果為何。例如選樣偏誤（selection bias，以實驗組和控制組的前測結果差異定義）導致組間有差異時，將組間差異歸因於實驗刺激就有問題。綜合以上兩點使得平均處理效果（實驗組平均組內差異－控制組平均組內差異）無法被正確評估，因此大法官釋字第748號解釋有嚴重的內在效度缺陷（無法反應到真正想測量的概念）。更簡單地說，如果立法程序的阻礙不僅僅出現在同性婚姻法案中，其他群體的群體利益以及更一般性（無特殊利益）的法案也如此時，同性戀真的有比較弱嗎？這本質上其實是一個更貼近經驗層面的問題。

第三個測量問題是大法官雖然努力地在解釋理由書第7、8段找出幾個同性戀婚姻法案請願和審議失敗的案例（或觀察值），但是為數不多（計算行政與立法領域下的案例僅8個），不僅並非母體所有的案例（例如欠缺「所謂弱勢者」的成功案例），若將此8個案例視為樣本，數目過少，欠缺變異，也難以向外推論至母體，外在效度欠佳，實有問題。

最後則是解釋中根本上的概念操作與理論問題。就概念操作而言，大法官以立法院議案的審議紀錄結果（政策結果）作為操作政治不平等與政治上無力者的構念出發點，不僅欠缺完整的概念化過程（說明其定義及依據）之外，「政治上無力者」這個構念是否還有其他層面？例如，有些議案、偏好甚至根本無法進入議程的現象，相對於同性戀議題頻頻得到關注成為社會矚目焦點且獲得同情與支持，前者就未被大法官納入解釋中討論成為觀察分析的對象。這部分就牽涉到經典的權力定義／面向問題，根據不同的權力面向與平等理論（如機會平等與結果平等），就會有不同的觀察指標。

最後在理論上，人數少的群體，就必定遭到國會議員的忽視嗎？那麼要如何解釋同志運動在世界各地出現的部分勝利與斬獲？大法官這類基於「人頭數目才是硬實力」的論述，其基本假設是一人一票票票等值，所有選民都同等重要，且政策偏好會隨著群體身份而區別。但是這裡面忽略了多元民主理論乃至於利益團體的存在、Olson所提出的集體行動的困境（牽涉組織動員因素）{Olson, 1965 #12395}，以及政治實力／權力在人群之中分配並不等的現實（例如富人可能具備更多的資源可運用，從而具備政治權力而能說服議員）。從上述對大法官釋字第748號解釋的批判，都顯示出更進一步探討的需要。

我根據權力的不同面向，從第一面向能在衝突中勝出的決策制訂權力{Dahl, 1957 #12587}至第二面向的議題設定權力{Bachrach, 1962 #12590;Bachrach, 1963 #12783;Bachrach, 1970 #12700}，提出幾個指標作為誰是政治上無力者的切入觀察角度：1.整體國會的政策決定（產出）回應誰的民意？2.國會議員的投票行為回應誰的民意？3.國會整體的政策議程回應誰的民意？4.誰透過政治參與表達更多的民意設定議題。並透過架構理論以探索探討可能的變因，平衡大法官的觀點，試圖將政治上的無力者的概念釐清。以下將會簡略就上述所提出的指標所涉及的領域簡單回顧。

# 政治上無力者的概念與理論基礎回顧

## 國會議員與國會整體的行為基礎

在國會研究中，解釋立法委員的行為時，幾乎都圍繞著立法委員的行為都是為了連任（或者更廣義地說，為了使其政治生涯能夠維持）的公理而出發{Mayhew, 2006 #12913}。在這樣的前提下，大法官所提出的假說「立法者為了不得罪選民會傾向忽視少數群體的利益」或許言之成理。然而，這樣的前提預設並不完全能夠對應到現實世界中的各種條件。

首先，Fenno{Fenno, 1973 #12910}就指出國會議員基本上有三個行為準則：尋求連任、在國會能夠有影響力及制訂好的公共政策。雖然尋求連任本身是幾乎可以說是最終的目的，其他行為準則「只是輔助」，但在尋求連任並不成為太大問題的前提下，國會議員本身也有動能尋求制訂好的公共政策。

在連任方面，國會議員為了連任，不一定傾向將選票極大化，倘若國會議員已經建立好舒適的個人選票（personal vote），無論是選區特性使然（例如我國的金門、苗栗等地；或者如地域主義性質極強的韓國，以全羅道為例：自由派與保守派得票率往往是百分之九十幾與個位數得票率的差別）、或者選民特性使然──例如，政策回應只是代表的其中一個面向{Eulau, 1989 #12909}，選民傾向以議員的服務回應或分配回應評估候選人{Griffin, 2010 #12873}，因此其重要性超越政策回應／象徵代表時──、或者競爭對手過弱，或者議員資源豐沛經營選區得當……等等，一旦議員跨過安全當選的閾值時，此時國會議員理論上也會有空間朝向另外的目標努力。

此外，若議員產生方式並非透過選舉而是由政黨提名（如不分區名單），或者是決定議席方式中政黨選票（party vote）的成分與重要性大於個人選票時（例如選制上採複數選區單一投票不可讓渡制SNTV使得議員無須爭取選票極大化），或者選民的投票抉擇因素中政黨認同的強度可能蓋過政策認同時，議員的行為也可能轉向爭取個人在政黨的影響力{Carey, 1995 #12908}{Hirano, 2006 #12911}以及制訂好的公共政策，此時建立個人選票的重要性即不如想像中重要。

其次，國會議員的行為準則雖然理論上（理想層次中）需要透過不斷回應民意，以期爭取選民的認同與支持以避免選民在下次投票中會受到選民的懲罰，但實際上選民與議員雙方間這種民意溝通交換與回應及問責的連線並不一定能夠完美連接。現實世界中的選民可能是完全不過問政治、無法參與也無意願參與政治的「非理想公民」，這也就是Berelson所稱的「民主的弔詭」（paradox of democracy）{Berelson, 1954 #12912}。Miller及Stokes的研究{Miller, 1963 #12740}就發現，國會議員會隨著議題的不同而有不同回應選民的程度，在選民較陌生，難以認知、理解、充分掌握資訊或者較無興趣而欠缺注意、沒有清楚立場的領域中（例如外交國防），國會議員較有充分自主的空間，因而相對於較與人民相關、民意較可能關注、敏感度或張力較強的社會福利與民權領域，國會議員回應民意的程度較低。此種情形下，若又無堅強的以群眾為基礎的利益團體代表選民反應民意與利益或提供資訊時，不僅議員無法感知民意，民意也無從監督從而透過選舉課責，「人頭就不是硬實力」，議員不是參考其他的影響力來源行事（例如政黨、行政機關、同黨議員、政黨國會領導人），就是依據自身的意志及自己對於政策的評估行事。

回應有政治資源的人。

## 誰決定公共政策的制訂

在憲法領域，關注民主失靈程序的傳統反多數決理論中，因其論述較著重「偏見」的角色，多從議員及其選民存有的偏見去推測「因為偏見而導致失靈的產生」從而證成違憲審查，在其理論與概念上政策決定的政治行動者僅有國會議員──政治菁英，而影響力的來源則僅有選民以及政治菁英自己。但是單是從議員基於連任考量必須時時回應民意這一點並不能完全解釋政策產出與政策回應。如同前述，選民可能根本上欠缺意識、並未形成意見或者無能也無力參與政治，此時民意的來源（政策的需求輸入端）就可能來自有資源有權力者以及組織良好的利益團體，議員可能根本無從得知選區的「多數民意」具體形貌為何。另外，基於選區的異質性，如果某一政策回應雖然可以博取選區多數選民的認同與利益，甚或是符合議員自己對公共政策的評估，卻會得罪另外一群人時，議員甚至有可能採取策略性回應，例如僅象徵性回應{Eulau, 1989 #12909}，也可能策略性設定議程，透過議程選擇而暫時將爭議壓在台面下，試圖壓抑尚未浮出、高漲的民意{Cox, 2005 #12863}。在此種情形下，不同政策主張的出現，也有可能是政黨、利益團體動員從而帶領民意的結果，也有可能根本就是議員（特別是在野黨）和利益團體結合或者是為了與競爭對手打對台而根本上有策略而刻意地設定議題而試圖帶領著民意走向{Brunner, 2013 #12838}並試圖創造政策流。此時政策決定的要角，自然是國會議員這些政治菁英，而政策的制訂自然也就可能傾向立法委員自身的利益與價值作為判斷基準，並非由選民做為政策輸入端。

更經典的晚近研究是Gilens&Page{Gilens, 2014 #12679}利用大規模的政策議題檢驗經濟菁英、利益集團與一般公民誰統治／誰有權力（Who governs?）的問題。他們首先整理出關於「誰決定美國的公共政策走向」的四種傳統理論：多數選舉民主論（Majoritarian Electoral Democracy）、經濟菁英支配論（Economic-Elite Domination）、多數多元論（Majoritarian Pluralism）以及偏差多元論（Biased Pluralism）。多數選舉民主論主張政治人物都會向中位選民的政策偏好靠攏，因為這樣的政策偏好最能符合多數選民的偏好，例如Anthony Downs的*Economic Theory of Democracy*所主張，同時也因為符合最多數選民的偏好而有在規範上符合民主精神。經濟菁英支配論則主張實際上由擁有經濟資源者主導了美國的政策制訂，如C. Wright Mills的*The Power Elite*、G. William Domhoff的著作*Who Rules America?*中指出經濟菁英透過基金會、智庫、遊說、贊助候選人等機制塑造輿論的機制主導了公共政策的走向。多數多元主義論則將焦點著重在由不同的團體競逐公共政策的現象，包含黨派、利益團體、企業、產業部門等，如James Madison的聯邦論第10講（*Federalist Paper No. 10*）就曾經談到派系（factions）在政治上的作用。David Truman的*The Governmental Process*、Robert Dahl的*Who Governs?*、*Polyarchy*等也都詳細並討論了多元論。偏差多元論則是從多元論中未能關注Mancur Olson的「集體行動的困難」的論點出發，當每個個人的小利益難以透過組織集結起來，無法克服團體的組織與動員成本時，個人傾向搭便車而由他人負擔行動成本坐享漁翁之利，包含E.E. Schattschneider的*The Semisovereign People: A Realist's View of Democracy in America*認為美國的利益團體由組織化的特殊利益群體構成，偏向上層階級，群體成員也與經濟社會地位相關。Kay Lehman Schlozman, Sidney Verba, and Henry Brady等人{Schlozman, 2018 #12916;Schlozman, 2012 #12650;Verba, 1995 #12621}對於政治參與的研究也發現經濟社會地位較低者政治參與程度較低的現象，顯示多元競爭的現況並非對稱，反而其均衡點是往有豐沛經濟資源的一方偏的。

在比較這幾種理論後，經過實證研究分析，他們發現經濟菁英、代表企業利益的利益團體實際上帶領了政策的走向，相對地一般公民以及群眾為組成基礎的利益團體幾乎沒有影響力，在理論上也因此證明了經濟菁英支配論與偏差多元論較佔上風，而多數選舉民主論以及多數多元論較欠缺證據支持。

## 小結：政治上無力者的建構與假設提出

綜合以上理論探討，其根本差異在於不同的民主制度運作經驗描述、不同的公共政策產出運作模型，以及哪一個政治行動者對於政策結果而言具有較大影響力而扮演較重要的角色。

回到先前我所提出的觀察誰是政治上無力者的幾個指標：1.整體國會的政策決定（產出）回應誰的民意？2.國會議員的投票行為回應誰的民意？3.國會整體的政策議程回應誰的民意？4.誰透過政治參與表達更多的民意設定議題。此處先略過第四個指標不談，經由以上文獻回顧，本研究要驗證的理論，包含：

* 菁英觀點，接近憲法學者Ely一派代議補強論的角度，其假說認為國會議員會因為偏見或歧視或觀點不一致，特別在選民屬於少數群體的情形下，基於選票考量或自身的利益、偏見與背景評估會越不回應選民的意見；而由於偏見的產生是因為對外群體的刻板印象所致，議員與選民的特徵、背景不相似，會有越低的敘述代表性，敘述代表性越低者其選民政治上越無力，選民所屬群體人口越低時政治上越無力。依照此理論，用以驗證的虛無假設為：
    + 整體國會的政策產出與議員的政策回應，在多數群體與少數群體間並不會有不同程度的回應。
    + 不論選民與議員之間的敘述代表（相似）性如何，議員回應選民的程度一致。
    + 沒有選票壓力的議員（全國不分區議員），回應少數群體與多數群體選民的程度與區域議員相同。
* 多元論觀點，其假說認為選民可以動員影響立法委員，議員無法自己作主，即便可以，空間也有限。動員越強或參與政治程度越高時，國會議員受選民壓力越高越回應選民，影響力越低者（通常可能是參與政治程度越低者）越弱勢。根據此理論，用以驗證的虛無假設為：
    + 不論選民參與政治程度為何，立法委員回應的程度一致。
* 經濟菁英支配論與偏差多元論觀點，其假說認為社會經濟地位高的人掌握了整體的政治程序權力，國會的政策的制訂以及議員的行為均回應這些人的政策偏好。根據此理論，用以驗證的虛無假設為：
    + 不論選民社會經濟地位為何，國會的政策產出以及議員回應的程度一致。

# 研究設計

## 資料來源與處理

本研究的研究對象聚焦於立法院的政策決定及立法委員的投票表決行為「回應誰的民意」，並且將民意與回應二者間的對應範圍限縮至問卷調查做成後一年半至二年內的立法委員投票表決行為以及國會透過投票通過的決議。

首先，我透過向[SRDA學術調查研究資料庫](https://srda.sinica.edu.tw)申請臺灣社會變遷基本調查「2004第四期第五次公民權組」、「2010第六期第一次：環境組」、「2010第六期第一次：綜合組」、「2016第七期第二次：公民與國家組」的限制版資料，限制版的資料與一般版不同處在於包含有受訪對象的戶籍地村里層級資料，藉此可以準確地與選區資料合併。這幾個資料集中均包含受訪對象對政策的偏好與看法可作為民意資料來源。同時我將各問卷中重複、相同的問題以「2016第七期第二次：公民與國家組」的過錄編碼簿標準合併。合併完成後，將「漏答;忘記了,不知道;拒答」等選項視為遺漏值，運用R語言的mice套件{van Buuren, 2011 #12742}，以random forest為填補演算法進行遺漏值的多重填補方法填補。

研究上選擇這四組調查樣本，第一是因為這四組資料均同時含有基本資料（人口變項）、政策議題調查以及政治參與的資料，第二是因為這四組資料的政策意向問題較多，涵蓋層面較廣而全面（2010年兩組樣本的政策意向問題合併後共有101題、2016公民與國家組的政策意向約共有76題），能夠貼近人們的政策偏好不僅限單一面向或群體利益導向政策的情形，同時對於研究工作而言也能夠在一次的議案編碼後對應比較多問卷調查題目而比較有效率，研究發現與結論有較高的效度。

另外，在立法委員的投票表決行為如何回應民意的資料上，也採取「控制選區」的作法，也就是各個受訪者的意見只與其所處選區的立法委員表決資料串連。控制選區的重要性在於不同的選區會有不同的議題導向與利益需求，例如農業縣與都會商業區可能選民意向以及利益就截然不同。為了排除此項因素的干擾，更聚焦於選民對於立法委員的影響，此處會對於選區控制區別。

首先利用[立法院議事暨公報管理系統](https://lci.ly.gov.tw/LyLCEW/lcivAgendarecMore.action) 檢索立法院院會的議事錄網址，以R軟體爬蟲取得各個議事錄，接著將議事錄文件結尾處的記名表決結果名單內容結構化處理，並透過比對議事錄內容中記名表決關鍵字（以正規表達式搜尋）以及事後檢查檢核並修正，形成一個包含有立法院屆次、會期、會議次、臨時會次、會議時間、投票表決議案編號、立法委員姓名與投票決定（包含贊成、反對、棄權、未出席、未投票）的立法院表決紀錄資料集^[原本研究要利用[g0v的「立委投票指南網站」](https://vote.ly.g0v.tw/)的資料集，但後來發現有錯誤，於是僅部分參考其公開的程式碼中的演算法與靈感，並修正其錯誤]。

接著與[立法院開放資料服務平台提供的歷屆委員資料](http://data.ly.gov.tw/listcatelog.action?catCd=2) ──包含屆別、姓名、性別、黨籍、黨團、選區名稱、學經歷──等屬性的資料集以及[中央選舉委員會的選舉資料庫資料集](https://data.gov.tw/dataset/13119)、[選舉資料庫](http://db.cec.gov.tw/histCand.jsp)、[歷史選舉公報](http://bulletin.cec.gov.tw/bin/home.php)串連以增加立委個人基本資料的欄位。

在民意調查資料中，將整個民意資料集以各個政策意向的問題為對照基礎，將資料從短資料轉換為長資料──也就是一個觀察值從代表一個受訪者的全部變數，轉換為一位民眾就一個政策議題表達意向的全部變數。

隨後我依據表決紀錄／表決議案的資料集建立與議案與民意之間的關係以及議案屬性。在建立完成關連之後，透過SPSS軟體將資料檔輸出過錄編碼簿（也就是每一個問題有哪些答案的資料集），接著將此過錄編碼簿串連前述表決議案資料集，並且針對每一個議案與答案選項的關連編碼出「議案的立場」。

特別需要說明的是某些表決議案涉及多方向利益角力的編碼方式，舉兩個代表案例說明，首先是[立法院第7屆第6會期第14次會議第24表決議案](https://lci.ly.gov.tw/LyLCEW/html/agendarec/02/07/06/14/LCEWC03_070614.htm)，涉及的是當時二代健保的補充保費修正案，表決結果決定新增補充保費，費用來源為獎金、兼職薪資所得、執行業務收入、股利所得、利息所得、租金收入（被稱為林志玲條款）。相對應的題目是2010綜合問卷的「您是不是贊成如果大家的收入更平均的話,一般人會因此更不努力工作的說法?」（回答：1非常贊成;2贊成;3不贊成;4非常不贊成;……其他遺漏值）以及「有人說:減少高收入與低收入之間的差距,是政府的責任,請問您同不同意?」（回答：1非常同意;2同意;3無所謂同不同意;4不同意;5非常不同意;……其他遺漏值）。補充保費的決定實施看似比原先舊制更往所得重分配的方向走，但如果檢視立法過程中的對案可以發現反對所得重分配的利益並沒有全輸，因為反對陣營的對案中的「退職金、海外所得、買賣資產所得」並未被納入，在此種情形下，我的編碼方式是將此議案與兩個問題建立兩次的關連，接著在「議案的立場」方面，第一次的關連編碼回應了贊成所得重分配者的意見，第二次的關連編碼回應了反對所得重分配者的意見。同樣的情形發生在[立法院第9屆第2會期第13次會議第2表決議案](https://lci.ly.gov.tw/LyLCEW/html/agendarec1/02/09/02/13/LCEWC03_090213.htm)，涉及的是勞基法一例一休修法中以休息日加班要發加倍加班費的改變，相關連的題目是2016公民的「請問您是贊成還是反對?f透過減少每個人的工作時數,讓更多的人可以工作」（回答1很贊成;2贊成;3既不贊成也不反對;4反對;5很反對;……其他遺漏值）及「應不應該是政府的責任?e為工商業成長所需提供協助」（回答1當然應該;2還算應該;3不太應該;4當然不應該;……其他遺漏值），議案本身表面上看起來與舊制相比回應了贊成減少工時者以及認為政府不應該透過減少工時協助工商業成長者的意見，但檢視對案可以看到這方面的利益也並未完全勝利，因為「二例假日」的訴求並未成功，在此情形下也一樣建立兩次關連，而有關「議案的立場」則分別給予相反的編碼。

在上述資料集處理完成後分別將各個資料集串聯中央選舉委員會在政府資料開放平台釋出的[選舉資料庫](https://data.gov.tw/dataset/13119)（並且修正若干錯誤）後將各個資料集增加選舉屬性資料。接著，選擇各個民意調查做成後一年期間（也就是2010年7月至2011年6月、2016年8月至2017年5月）內的表決紀錄資料，以這段期間的資料為準再將資料串連合併。串聯合併條件分別是將選區選民（受訪者）和該選區立法委員串在一起，以及不分選區將所有立法委員和所有選民串在一起，這也分別就是立法委員代表全國選民以及立法委員代表選區選民的情形，同時也落實前面研究設計所提到的控制選區的問題；串聯方式為inner join，也就是僅留存有共同欄位的值相同的觀察值。總計在此期間內有430個表決議案可以對應到問卷的問題。

## 變項與分析模型的建立

立法委員資料：

* 立法委員敘述代表性：透過下列相關變數，使用R語言的gllvm套件，以Generalized Linear Latent Variable Model縮減出一個連續變數資料，作為操作敘述代表性的指標，數值越低敘述代表性越高（議員與選民特徵越相似）：
    + 立法委員性別與選民性別是否一致：根據中選會資料、立法院的立法委員個人資料、調查資料的受訪者性別判斷編碼。一致編碼為0，不一致編碼為1，為測量敘述代表性的類別變項。
    + 立法委員受教育年數與選民受教育年數差距：根據中選會資料與立法院的立法委員個人資料及問卷受訪者的回答資料，並且依照學位等級進行編碼，以國中9、高中/高職12、五專/二專/三專/學士14、技術學院/大學/學士16、研究所/碩士19、博士23編碼。以立法委員受教育年數，減去選民的受教育年數後，取絕對值。為測量敘述代表性的連續變項。
    + 立法委員職業社經地位與選民職業社經地位差距：以立法委員院外／從政前職業社經地位，依據立法委員主要的院外職業或是擔任立法委員前主要的經歷與職業，參考{黃毅志, 2008 #12731}編碼得出連續變項。盡量蒐集並依據最早期從政參選時的選舉公報資料為主。若僅有黨職、聯誼性社團經歷而無其他經歷時，則定位其職業為民意代表，大部分有此類特徵者在選舉公報上面的職業都記載社會服務或是政治工作，地方派系勢力、年輕從政的政二代也多是此模式；如果是從政治人物幕僚開始一路往上爬的政治人物，未經過國會助理（不包含國會辦公室主任此一管理職）或是智庫研究員歷練的也編入民意代表。完成立法委員院外社經地位編碼並減去選民的職業社經地位後，差額取絕對值。為測量敘述代表性的連續變項。
    + 立法委員族群與選民族群是否一致：依據選舉公報、新聞、Google網頁搜尋所得資料進行立法委員族群編碼，客家政治人物資料來源並參考{何來美, 2017 #12749}，分為台灣閩南人、台灣客家人、大陸各省市、台灣原住民、外裔或原國籍為外籍或原國籍中國大陸的新移民、前述分類以外的臺灣人。編碼後與選民族群變項進行判斷，一致編碼為0，不一致編碼為1，為測量敘述代表性的類別變項。
    + 立法委員與選民年齡差距：以立法委員的年齡減去選民的年齡後，取絕對值。為測量敘述代表性的連續變項。

民意資料：

* 各人口變項：包含
    + 性別。
    + 年齡。
    + 族群：四個問卷的問題皆有共同的「父母親是哪裡人」的兩個選擇題及開放填充題，我將此題目依據答案重新編碼為父母親分別為台灣閩南人、台灣客家人、大陸各省市、台灣原住民、外裔或原國籍為外籍或原國籍中國大陸的新移民、前述分類以外的臺灣人（例如有部分受訪者的父母親是「外省人第二代」，而在開放填充題中回答臺灣人）。接著依照廣義的原生論（primordialism），只要父母有其一為人口比例較少的較少數族群者，則一律視為較少數族群。為類別變項。
    + ~~所屬族群人口比例：將前述的族群類別變項，依據各族群佔全國人口比例編碼為數值的連續變項。人口比例的資料來自於行政院客家委員會委託研究的「[99年至100年全國客家人口基礎資料調查研究」以及「105年度全國客家人口暨語言調查研究報告](https://www.hakka.gov.tw/Content/Content?NodeID=626&PageID=37585)」、[內政部戶政司人口資料庫](https://www.ris.gov.tw/zh_TW/346)、[內政部移民署業務統計資料](https://www.immigration.gov.tw/lp.asp?ctNode=29699&CtUnit=16434&BaseDSD=7&mp=1)。~~
    + 教育程度：以受訪者填答的教育程度，綜合其學位等級轉換為受教育年數為準的數值編碼，轉換標準為：無/不識字/識字/私塾/自修0^[外某受訪者回覆日本教育讀三年，經比對檢視其他回覆內容後以0編碼]
；小學6；國(初)中、初職 9 ^[某受訪者回覆空軍子弟小學的師範學校，經檢視其他回覆內容後以9編碼]；高中、綜合高中、高職、高中職業科、高中普通科、中正預校 12 ^[某受訪者回覆宜蘭特教學校，比對該受訪者對於「請問您從國小到現在,總共受幾年的學校教育?」題目反應為12，並經查證宜蘭的特教學校有高中職學制，因此編碼為12]；軍警校專修班、軍警專修班 13；士官學校、五專、二專、二專、三專 14；軍警校專科班、軍警專科班、空中行(商)專；空中大學、軍警官學校/大學、軍警官校或大學、技術學院、科技大學、二技、四技、大學 16 ^[某受訪者回覆基督學院，經查證後該校授與學士學位，同樣編碼為16]；碩士 19；博士 23。
    + 家庭收入：主要依據為問卷問題為「包括各種收入來源,您全家人的所有收入,每個月大約多少元?」，原調查得到的資料為根據每組不同所得範圍的區間組別，此處重新根據各組組中點的編碼為收入，最低一組（無收入）為0，最高一組（100萬元以上）編碼為150,000。
    + 職業社經地位：以各問卷中的「（退休前或未退休）工作主要的職位和工作內容是?變遷職位碼」問題參考{黃毅志, 2008 #12731}轉換得出一職業社經地位的量化連續尺度變項。
* 綜合社經地位／階級：以因素分析法從前述教育程度、個人收入（2010綜合問卷題項為工作收入，此處同樣以個人收入定義）、家庭收入及職業社經地位等變項，以因素分析法萃取出共同因素（主軸因素），過程運用由R語言中的factanal函數，該函數以最大概似法進行參數估計，並設定以最大變異法(varimax)轉軸，經Bartlett檢定均適合以一個共同因素進行因素分析。以下為各因素負荷量及變異解釋程度：
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
* 議題領域：分為經濟、公民與政治權、社會福利、財政、內政、兩岸、環境、經濟社會文化權。
* 同政策意向者佔全國／全選區整體意向比率：分別以全國（全調查樣本）／全選區（全調查樣本中同一選區的受訪對象）層級中與觀察對象持同方向政策意向者人數除以全部人數所得數值編碼。Likert五點或四點量表設計的問題中，將回答非常贊成與贊成某政策者編為同一類，非常反對／反對某政策者編為同一類。這個指標也就是要衡量觀察對象的政策意向連結整體層級的民意強度。
* 意向強度：如果題目問題設計以Likert五點或四點量表設計的問題中，贊成、反對等選項編碼為1，非常贊成、非常反對等選項編碼為2。若題目設計僅有三等（贊成、無意見、反對）或強迫回答的二等時，均編碼為1，其餘無意見編碼為0，為序數尺度變項。
* 是否為重視議題：對於表決議案內容分析，如果議案內容屬受訪者覺得重要者，編碼為1，否則編碼為0的類別變項。是否重要以2004公民組的「今後十年的國家目標中,您認為哪個最重要?」、「今後十年的國家目標中,您認為哪一個次要的?」 ^[答案：高度的經濟成長、保護本國有強大的國防力量、看到人們在他們的工作中有更多的發言權、盡量使我們的都市和鄉村更美麗] ；2010環境組的「請問您認為以下哪個項目是目前台灣社會最重要的議題?」、「哪一項議題是目前台灣社會第二重要的?」 ^[答案：健康照顧、教育、犯罪、環境、移民、經濟、恐怖主義、貧窮、以上皆非] 進行判斷；
* ~~工作狀態：略~~
* ~~政治效能感：分為外在效能感（個人感覺政府有去回應個人需求的能力）與內在效能感（個人感覺自己有去瞭解及參與政治的能力），依據問卷不同的題目作為政治效能感的指標，如指標包含不只一題，則以各題分數加總之後平均計算。操作方式如下表：~~

* ~~相對剝奪感：以受訪者對對現狀的評價為依據，主要題目為還沒編。「您覺得政府現行的環境保護政策符不符合公平正義的原則?」~~
* ~~政治興趣：d15 請問您個人對政治有沒有興趣?（2016公民）~~
* 非競選期間政治參與：依據原本問卷設計中的下述次序變項，以應用試題反應理論（item response theory）的 graded response model 計算測量出一個潛在連續型態的潛在變數，測量過程使用R語言的mirt套件{Chalmers, 2012 #12917}，括弧內為因素負荷量：
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> 2004公民 </th>
   <th style="text-align:left;"> 2010環境 </th>
   <th style="text-align:left;"> 2010綜合 </th>
   <th style="text-align:left;"> 2016公民 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> v28 您有沒有做過請願(簽名)連署(0.815) </td>
   <td style="text-align:left;"> v34 請問您是環保團體的成員嗎?(0.712) </td>
   <td style="text-align:left;"> v79a 在過去的一年,您有沒有向政府官員,民意代表或政黨反映意見提出要求?(0.743) </td>
   <td style="text-align:left;"> h2a 您過去有沒有做過或將來會不會做這些事?a 請願(簽名)連署(0.796) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> v29 您有沒有因為政治,倫理道德,或是環保的理由拒絕購買或是特別去購買某些產品(0.623) </td>
   <td style="text-align:left;"> v35a 在過去五年間,請問您有沒有做過以下的事情:連署一份有關環保議題的請願書(0.699) </td>
   <td style="text-align:left;"> v79b 在過去的一年,您有沒有向大眾媒體投訴?(0.976) </td>
   <td style="text-align:left;"> h2b 您過去有沒有做過或將來會不會做這些事?b 因為政治的、倫理(道德)的、或是環保的理由拒絕購買或是特別去購買某些產品(0.654) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> v30 您有沒有參加示威遊行(0.840) </td>
   <td style="text-align:left;"> v35b 在過去五年間,請問您有沒有做過以下的事情:捐款給環保團體(0.704) </td>
   <td style="text-align:left;"> v79c 在過去的一年,您有沒有透過網際網路反映意見?(0.512) </td>
   <td style="text-align:left;"> h2c 您過去有沒有做過或將來會不會做這些事?c 參加示威遊行(0.885) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> v31 您有沒有參加政治集會或造勢活動(0.777) </td>
   <td style="text-align:left;"> v35c 在過去五年間,請問您有沒有做過以下的事情:參加有關環保議題的抗議行動或遊行(0.603) </td>
   <td style="text-align:left;"> v79d 在過去的一年,您有沒有參加遊行,示威,靜坐或其他自力救濟方式?(0.633) </td>
   <td style="text-align:left;"> h2d 您過去有沒有做過或將來會不會做這些事?d 參加政治集會或造勢活動(0.674) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> v32 您有沒有找過政治人物或公務人員表達您的看法(0.785) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2e 您過去有沒有做過或將來會不會做這些事?e 找政治人物或公職人員表達您的看法(0.746) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> v33 您有沒有捐錢給某個社會或政治活動,或者幫他們募款(0.562) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2f 您過去有沒有做過或將來會不會做這些事?f 捐錢給某個社會或政治活動,或者幫他們募款(0.656) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> v34 您有沒有透過媒體去表達您的看法(0.810) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2g 您過去有沒有做過或將來會不會做這些事?g 透過媒體去表達您的看法(0.812) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> v35 您有沒有參加網路上的政治論壇或討論群組(0.725) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h2h 您過去有沒有做過或將來會不會做這些事?h 透過網路表達您的政治想法(0.781) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> v36 請問您有沒有加入和參與政黨(0.394) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3a 您過去有沒有做過或將來會不會這樣做?a 跟親朋好友討論不公不義的事情或現象(0.583) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> v37 請問您有沒有加入和參與工會,工商同業公會,職業同業公會(0.240) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3b 您過去有沒有做過或將來會不會這樣做?b 參加抗議不公不義的活動(0.886) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> v38 請問您有沒有加入和參與宗教團體或教會(0.396) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> h3c 您過去有沒有做過或將來會不會這樣做?c 以金錢來支持弱者或公義團體(0.521) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> v39 請問您有沒有加入和參與運動,休閒或文化團體(0.512) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> v40 請問您有沒有加入和參與其他自願性社團(0.537) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 可解釋變異比例 </td>
   <td style="text-align:left;"> 0.415 </td>
   <td style="text-align:left;"> 0.464 </td>
   <td style="text-align:left;"> 0.542 </td>
   <td style="text-align:left;"> 0.541 </td>
  </tr>
</tbody>
</table>
* 統獨傾向：依據問卷中的以下問題類別變項，使用以潛在類別模式（Latent class model）為原理的R語言套件poLCA{Linzer, 2011 #12918}，預測／估計受訪者潛在的統獨傾向，經檢視BIC值後發現設定三個潛在類別的模型適配度均最佳，分為「統一」、「中立」、「獨立」三類。此變項並非分析模型內的變項，而是用來與立法院的統獨議案及表決紀錄比較之用。2010環境並無統獨傾向問題所以並未建立變項、2016公民並不使用潛在類別模式估計，直接以題目回答結果編碼；括弧內為潛在類別模式估計外顯類別對潛在類別的機率（比率）：

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
   <td style="text-align:left;"> v95 關於台灣和大陸的關係,您的看法是什麼 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v90 就兩岸關係而言,請問您覺得台灣獨立,統一,維持現狀何者比較好? </td>
   <td style="text-align:left;"> h10 關於台灣和大陸的關係,這張卡片上有幾種不同的看法,請問您比較偏向哪一種? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v96 您同不同意若台灣宣布獨立後,仍可和中共維持和平,則台灣應該成為一個新國家 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v91 請問您同不同意若台灣宣佈獨立,仍可和中共維持和平關係,則台灣應成為一個新國家? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> v97 您同不同意若大陸和台灣在經濟,社會,政治各方面的條件相當,則兩岸應該統一 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v92請問您同不同意若大陸和台灣在經濟,社會,政治各方面的條件相當,則兩岸應該統一? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>


* 政黨傾向：依據問卷中的以下問題類別變項，使用以潛在類別迴歸模式（Latent class regression model）為原理的R語言套件poLCA{Linzer, 2011 #12918}，並以前述統獨傾向外顯變項為共變項，預測／估計受訪者潛在的政黨傾向，分為「泛藍」、「中立」、「泛綠」三類。2010環境並不使用潛在類別模式估計，直接以題目回答結果編碼；括弧內為潛在類別模式估計外顯類別對潛在類別的機率：
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> 2004公民 </th>
   <th style="text-align:left;"> 2010環境 </th>
   <th style="text-align:left;"> 2010綜合 </th>
   <th style="text-align:left;"> 2016公民 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> v88 請問您這次(今年三月20日)的總統選舉投票給誰 </td>
   <td style="text-align:left;"> v103 目前國內有幾個主要政黨,請問您有沒有比較偏向哪一個政黨? </td>
   <td style="text-align:left;"> v84 請問您記不記得投給那一位總統候選人? </td>
   <td style="text-align:left;"> h5 請問您投票給哪一組候選人?(提示卡29) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> v89 請問您上一次(2000年)總統選舉投給誰 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v86請問您在區域選舉部分是投給那一黨的立法委員候選人? </td>
   <td style="text-align:left;"> h6r 請問在這次區域立委選舉,您投給哪一個政黨的候選人? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> v90請問您上一次(民國九十年)立法委員選舉投給哪一黨的候選人? </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v87a1請問您記不記得投給那一位立法委員候選人?候選人政黨代碼 </td>
   <td style="text-align:left;"> h7下面我們列出這次參加不分區立委選舉的政黨,請問您把票投給哪一個政黨? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> v98 請問您是否偏向哪一個政黨 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v88那在政黨票部分,您是投給哪一個政黨? </td>
   <td style="text-align:left;"> h8國內的政黨都有他們的支持者,請問您是哪一個政黨的支持者? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> v99請問您覺得自己是偏向(泛綠)陣營,還是(泛藍)陣營? </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v93 在國民黨,民進黨,親民黨,台聯,新黨這五個政黨中,您認為您比較支持哪一個政黨呢? </td>
   <td style="text-align:left;"> h9一般而言,請問您會比較偏向哪一個政黨? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> v94您比較偏向國民黨,偏向民進黨,偏向親民黨,偏向台聯,偏向新黨,或是都不偏? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 可解釋變異比例 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

合併民意資料與議案及立法委員議案表決紀錄資料後，建立應變項如下：

* 立法委員是否回應民意：對照前述處理資料所建立的「議案的立場」，若選民的立場與議案的立場一致，而立法委員投票贊成時，編碼為2（回應），投票反對時編碼為0（拒絕）；選民的立場與議案的立場相反而立法委員投票反對時，編碼為2（回應），投票贊成時編碼為0（拒絕）；投下棄權票、立法委員未出席會議、出席會議但未投票，編碼為1（棄權）。「拒絕」、「棄權」以及「回應」，有順序關係，此為一有順序（ordinal）關係的類別變項。
* 表決結果的議案是否回應民意：針對議案的表決結果，觀察其政策立場是否與選民的立場一致。如果議案立場與選民一致且表決通過時，編碼為1；立場一致但表決不通過，編碼為0；立場不一致但表決通過時，編碼為0；立場不一致而表決不通過時，編碼為1。此為二元類別變項。
* 議程設定是否回應民意：首先選取通過三讀的法案以及曾經經由表決擋下其議程往前推進（如自委員會抽出逕付二讀）或致使倒退（例如退回程序委員會）的法案作為觀察對象，如果法案立場與選民立場一致且通過三讀或者是其議程推進表決通過或議程倒退表決不通過時，編碼為1，反之編碼為0；立場不同時，如通過三讀或者是其議程推進表決通過或議程倒退表決不通過時，編碼為0，反之編碼為1。

控制變項上，則有：

* 政黨團結程度：參考{黃秀端, 2006 #12613}、{盛杏湲, 2008 #12903}的作法，依據一表決議案中與觀察立法委員的同黨籍立法委員中，與觀察對象做出相同立場投票（決定）在其所屬政黨中的比例，減去同黨籍立委中其他投票立場的比例。此變項其實也相當於立法委員所受到的來自政黨的壓力／政黨動員比率／政黨施壓率。所謂同黨籍立法委員的其他立場決定，也把未出席院會、棄權、出席院會但不投票也視為一種決定。

每一個觀察值代表一個意見，共計觀察值數目：


資料檢核後，各變項遺漏值如下：


關於「立法院通過的政策決定，回應誰的民意」以及「議程設定是否回應民意」部分，應變數為二元類別變項，因此使用logistic regression分析；關於「議員的表決是否回應選區選民民意」部分，由於應變項為0,1,2的次序變項，不同類別之間雖有次序關係但間隔可能不等距，迴歸分析使用ordinal logit models{Agresti, 2010 #12745}



## 除錯檢測

### 信度檢測

Cronbach’s α
內容分析部分抽樣核對

### 共線性檢測

共線性檢測：可以用correlation analysis相關係數矩陣、VIF test(超過10 drop)
http://r-statistics.co/Model-Selection-in-R.html （also tutorial on stepwise）

### 預測失準可能檢測

Heteroscedasticity: prediction必須要隨機分布，不能有pattern，否則導致模型失準； spot outliers

# 研究結果與發現
# 討論與結論
# 參考文獻

一、中文部分

* 何來美（著）（2017）。《台灣客家政治風雲錄》。臺北市：聯經。
* 盛杏湲（2008）。〈政黨的國會領導與凝聚力——2000年政黨輪替前後的觀察〉。《臺灣民主季刊》，第5卷第4期，頁 1-46 
* 黃秀端＆陳鴻鈞（2006）。〈國會中政黨席次大小對互動之影響——第三屆到第五屆的立法院記名表決探析〉。《人文及社會科學集刊》，第18卷第3期，頁 385-415 。 取自 https://www.rchss.sinica.edu.tw/app/ebook/journal/18-03-2006/cc1831.pdf
* 黃毅志（2008）。〈如何精確測量職業地位？——「改良版臺灣地區新職業聲望與社經地位量表」之建構〉。《臺東大學教育學報》，第19卷第1期，頁 151-159 

二、外文部分

* Agresti, Alan. (2010). Analysis of Ordinal Categorical Data (2 ed.). Hoboken: John Wiley & Sons 
* Bachrach, Peter＆Baratz, Morton S. (1970). Power and poverty: Theory and practice: Oxford University Press 
* Bachrach, Peter＆Baratz, Morton S. (1962). Two Faces of Power. American Political Science Review, 56(4), 947-952. doi: 10.2307/1952796
* Bachrach, Peter＆Baratz, Morton S. (1963). Decisions and Nondecisions: An Analytical Framework. American Political Science Review, 57(3), 632-642. doi: 10.2307/1952568
* Berelson, Bernard. (1954). Voting. Chicago: University of Chicago Press 
* Brunner, Martin. (2013). Parliaments and legislative activity motivations for bill introduction. Wiesbaden: Springer Fachmedien Wiesbaden 
* Carey, John M.＆Shugart, Matthew Soberg. (1995). Incentives to cultivate a personal vote: A rank ordering of electoral formulas. Electoral Studies, 14(4), 417-439. doi: https://doi.org/10.1016/0261-3794(94)00035-2
* Chalmers, R Philip (2012). mirt: A multidimensional item response theory package for the R environment. Journal of statistical software, 48(6), 1-29. 
* Cox, Gary W.＆McCubbins, Mathew Daniel. (2005). Setting the agenda : responsible party government in the U.S. House of Representatives. Cambridge; New York: Cambridge University Press 
* Dahl, Robert A. (1957). The Concept of Power. Behavioral Science, 2(3), 201-215. doi: 10.1002/bs.3830020303
* Ely, John Hart. (1980). Democracy and Distrust: A Theory of Judicial Review. Cambridge: Harvard University Press 
* Eulau, Heinz＆Karps, Paul D. (1989). The Puzzle of Representation: Specifying Components of Responsiveness, in H. Eulau＆J. C. Wahlke (Eds.), The Politics of representation continuities in theory and research (pp. 55-72). Ann Arbor, Michigan: U.M.I., Out-of-Print Books on Demand 
* Fenno, Richard F. (1973). Congressmen in committees. Boston: Little, Brown 
* Gilens, Martin＆Page, Benjamin I. (2014). Testing Theories of American Politics: Elites, Interest Groups, and Average Citizens. Perspectives on Politics, 12(03), 564-581. doi: 10.1017/S1537592714001595
* Griffin, John D.＆Flavin, Patrick. (2010). How Citizens and Their Legislators Prioritize Spheres of Representation. Political Research Quarterly, 64(3), 520-533. doi: 10.1177/1065912910373552
* Hirano, Shigeo. (2006). Electoral Institutions, Hometowns, and Favored Minorities: Evidence from Japanese Electoral Reforms. World Politics, 59(1), 51-82. doi: 10.1353/wp.2007.0017
* Mayhew, David R. (2006). Congress : the electoral connection. New Haven, CT; London: Yale University Press 
* Miller, Wakken E.＆Stokes, Donald E. (1963). Constituency Influence in Congress. American Political Science Review, 57(1), 45-56. doi: 10.2307/1952717
* Olson, Mancar. (1965). The logic of collective action : public goods and the theory of groups. Cambridge, Mass.: Harvard University Press 
* Schlozman, Kay Lehman、Brady, Henry E.＆Verba, Sidney. (2018). Unequal and unrepresented : political inequality and the people's voice in the new Gilded Age. Princeton: Princeton University Press 
* Schlozman, Kay Lehman、Verba, Sidney＆Brady, Henry E. (2012). The Unheavenly Chorus Unequal Political Voice and the Broken Promise of American Democracy. Princeton; Oxford: Princeton University Press 
* van Buuren, Stef＆Groothuis-Oudshoorn, Catharina Gerarda Maria. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of statistical software, 45(3), urn:issn:1548-7660. doi: 10.18637/jss.v045.i03
* Verba, Sidney、Schlozman, Kay Lehman＆Brady, Henry E. (1995). Voice and equality : civic voluntarism in American politics. Cambridge, Mass.: Harvard University Press 
* Young, Iris Marion. (1990). Justice and the Politics of Difference. Princeton, N.J.: Princeton University Press 


standardized beta coefficient

https://www.princeton.edu/~otorres/LogitR101.pdf
https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
https://www.jakeruss.com/cheatsheets/stargazer/












```r
##stargazer(model_influce_from_p_p_s.k.2,model_influce_from_p_p_s.k.3,model_influce_from_p_p_s.k.4,model_influce_from_p_p_s.k.5,model_influce_from_p_p_s.d.2,model_influce_from_p_p_s.d.3,model_influce_from_p_p_s.d.4,model_influce_from_p_p_s.d.5, title="立法委員回應民意與民意佔比、政黨成員意見佔比及及所屬政黨與執政黨間席次差距關係分析", align=TRUE, type = 'html', summary=TRUE, notes="model 1,2,3,4 為第七屆研究範圍期間,model 5,6,7,8 為第九屆研究範圍期間")
```

### 各種因素對議員投票的影響力




```r
#stargazer(model_influce_from_all_s.k.2,model_influce_from_all.d.2, title="立法委員回應民意與各因素關係分析", align=TRUE, type = 'html', summary=TRUE, notes="model 1 為第七屆研究範圍期間,model 2 為第九屆研究範圍期間")
```

### 政治參與對於議員是否回應的中介效果

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
## [1] huxtable_4.3.0   stargazer_5.2.2  kableExtra_0.9.0 dplyr_0.7.8     
## [5] knitr_1.20      
## 
## loaded via a namespace (and not attached):
##  [1] zip_1.0.0         Rcpp_1.0.0        rowr_1.1.3       
##  [4] highr_0.7         pillar_1.3.1      compiler_3.5.2   
##  [7] bindr_0.1.1       tools_3.5.2       digest_0.6.18    
## [10] lattice_0.20-38   evaluate_0.12     tibble_1.4.2     
## [13] nlme_3.1-137      viridisLite_0.3.0 pkgconfig_2.0.2  
## [16] rlang_0.3.0.1     openxlsx_4.1.0    rstudioapi_0.8   
## [19] yaml_2.2.0        bindrcpp_0.2.2    stringr_1.3.1    
## [22] httr_1.4.0        xml2_1.2.0        generics_0.0.2   
## [25] hms_0.4.2         grid_3.5.2        tidyselect_0.2.5 
## [28] glue_1.3.0        R6_2.3.0          rmarkdown_1.11   
## [31] tidyr_0.8.2       readr_1.3.0       purrr_0.2.5      
## [34] magrittr_1.5      backports_1.1.3   scales_1.0.0     
## [37] htmltools_0.3.6   assertthat_0.2.0  rvest_0.3.2      
## [40] colorspace_1.3-2  stringi_1.2.4     munsell_0.5.0    
## [43] broom_0.5.1       crayon_1.3.4
```
