###Do Not Change This
import urllib2,cookielib

def getNewsCount(term, begDate, endDate):
    bDate=begDate.split('/')
    eDate=bDate
    site='https://www.google.com/search?q='+term+'&biw=1280&bih=880&source=lnt&tbs=cdr%3A1%2Ccd_min%3A'+bDate[0]+'%2F'+bDate[1]+'%2F'+bDate[2]+'%2Ccd_max%3A'+eDate[0]+'%2F'+eDate[1]+'%2F'+eDate[2]+'&tbm=nws'
    hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
           'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
           'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
           'Accept-Encoding': 'none',
           'Accept-Language': 'en-US,en;q=0.8',
           'Connection': 'keep-alive'}
    req = urllib2.Request(site, headers=hdr)
    try:
        page = urllib2.urlopen(req)
    except urllib2.HTTPError, e:
        print e.fp.read()
    html = page.read()
    index=html.index("<div id=\"resultStats\">")+28
    return html[index:index+100].partition(' ')[0]






