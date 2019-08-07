import csv

arxiv_data = csv.reader(open("arxiv_data_full.csv", 'r', encoding="utf-8"), delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)
arxiv_data = list(arxiv_data)

from collections import defaultdict as dd

incidence = dd(lambda: dd(int))

domains_set = set()

edges = dd(lambda :dd(int))

for row in arxiv_data[1:]:
    names, day, month, year, abstract, link, tag, title, emails = row
    
    emails_li = emails.split('\t')
    domains = []
    for email in emails_li:
        if len(email) > 1 and '@' not in email and not email[0].isalpha() \
                          and not email[0].isdigit() \
                          and not email[-1].isalpha() \
                          and not email[-1].isdigit():
            continue
        
        
        domain = email.split('@')[-1].strip(",.{}[];()@$#")
        domains.append(domain)
        if ',' in domain:
            print(domain)
            continue
        
        for idx1, domain1 in enumerate(domains):
            for idx2, domain2 in enumerate(domains[idx1:]):
                src, tgt = sorted((domain1, domain2))
                edges[src][tgt] += 1
                

lines = []
with open("edges_weighted.csv", 'w', encoding="utf-8") as f:
    for src_idx, src_edge in enumerate(edges.keys()):
        if src_idx % 100 == 0:
            print(src_idx, len(edges.keys()))
        for tgt_idx, tgt_edge in enumerate(edges.keys()):
            lines.append(','.join([src_edge, tgt_edge, str(edges[src_edge][tgt_edge])])+'\n')

    f.writelines(lines)
