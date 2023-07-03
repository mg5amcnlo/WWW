import sys

def wrtab(value):
    """ n and nd are dummy arguments"""
    if isinstance(value, str) and not "0.0 " in value:
        text = value
    elif isinstance(value, str) and "0.0 " in value:
        text = "not req. "
    elif value == 0.:
        text = "not req. "
    else:
        text = "%5.3e" % value
    return "<td> " + text + " </td>\n"

# open the file with the results (passed as commandline argument)
results = open(sys.argv[1]).read().split('\n')

res_list = []

for res in [r for r in results if r and not r.startswith('#')]:
    res_dict = {}
    values = res.split()
###curstring>>iproc[i]>>icoll[i]>>iwdth[i]>>sqrts[i]>>mh[i]>>sc[i]>>pdf[i]>>xmuf[i]>>xmur[i]>>LO[i]>>NLO[i]>>NNLO[i]>>PDFLO[i]>>PDFNLO[i]>>PDFNNLO[i];
    res_dict['iproc'] = int(values[0])
    res_dict['icoll'] = int(values[1])
    res_dict['iwdth'] = int(values[2])
    res_dict['sqrts'] = float(values[3])
    res_dict['mh'] = float(values[4])
    res_dict['sc'] = int(values[5])
    res_dict['pdf'] = int(values[6])
    res_dict['xf'] = float(values[7])
    res_dict['xr'] = float(values[8])
    res_dict['LO'] = float(values[9])
    res_dict['NLO'] = float(values[10])
    res_dict['NNLO'] = float(values[11])
    res_dict['pdfLO'] = float(values[12])
    res_dict['pdfNLO'] = float(values[13])
    res_dict['pdfNNLO'] = float(values[14])
    res_list.append(res_dict)

# now look for central values
# a subprocess is identified by iproc, icoll, sqrts, mh, sc
subprocesses = []
for d in res_list:
    id_dict = {'iproc': d['iproc'],
                     'icoll': d['icoll'],
                     'iwdth': d['iwdth'],
                     'sqrts': d['sqrts'],
                     'mh': d['mh'],
                     'sc': d['sc']} 
    if id_dict not in subprocesses:
        subprocesses.append(id_dict)

new_res_list = []
for subproc in subprocesses:
    these_res = [r for r in res_list if \
            all([r[k] == subproc[k] for k in subproc.keys()])]
    central = [r for r in these_res if r['xr'] == 1 and r['xf'] == 1][0]
    central['LOd'] = min([r['LO'] for r in these_res])
    central['LOu'] = max([r['LO'] for r in these_res])
    central['NLOd'] = min([r['NLO'] for r in these_res])
    central['NLOu'] = max([r['NLO'] for r in these_res])
    central['NNLOd'] = min([r['NNLO'] for r in these_res])
    central['NNLOu'] = max([r['NNLO'] for r in these_res])

    new_res_list.append(central)


# now write the html
procname_dict = {4: "H<sup>--</sup>",
                 5: "H<sup>-</sup>",
                 6: "H<sup>+</sup>",
                 7: "H<sup>++</sup>",
                 202: "W'<sup>--</sup>",
                 203: "W'<sup>-</sup>",
                 204: "Z' (via WW fusion)"}

for res_dict in new_res_list:
    try: 
        cproc = procname_dict[res_dict['iproc']]
    except KeyError:
        cproc = ''

    text = ("<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" >\n" + \
           "<tr> <td><b> %s </b><td> LO &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>\n" + \
           "<td> NLO &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>\n" + \
           "<td> NNLO &nbsp;&nbsp;&nbsp;&nbsp;</td>\n" + \
           "<tr>\n" + \
           "<tr><td>&sigma;(pb):</td>\n") % cproc
    text += wrtab(res_dict['LO'])
    text += wrtab(res_dict['NLO'])
    text += wrtab(res_dict['NNLO'])
    text += "<tr> <tr> <td> theo err  +: </td>\n"
    for ord in ['LO', 'NLO', 'NNLO']:
        try:
            text += wrtab("%4.1f %%" % ((res_dict[ord+'u'] - res_dict[ord])/res_dict[ord]*100.) )
        except ZeroDivisionError:
            text += wrtab(0.)

#    text += wrtab("%4.1f %%" % ((res_dict['LOu'] - res_dict['LO'])/res_dict['LO']*100.) )
#    text += wrtab("%4.1f %%" % ((res_dict['NLOu'] - res_dict['NLO'])/res_dict['NLO']*100.) )
#    text += wrtab("%4.1f %%" % ((res_dict['NNLOu'] - res_dict['NNLO'])/res_dict['NNLO']*100.) )
    text += "<tr> <tr> <td> theo err  -: </td>\n"
    for ord in ['LO', 'NLO', 'NNLO']:
        try:
            text += wrtab("%4.1f %%" % ((res_dict[ord] - res_dict[ord+'d'])/res_dict[ord]*100.) )
        except ZeroDivisionError:
            text += wrtab(0.)
#    text += wrtab("%4.1f %%" % ((res_dict['LO'] - res_dict['LOd'])/res_dict['LO']*100.) )
#    text += wrtab("%4.1f %%" % ((res_dict['NLO'] - res_dict['NLOd'])/res_dict['NLO']*100.) )
#    text += wrtab("%4.1f %%" % ((res_dict['NNLO'] - res_dict['NNLOd'])/res_dict['NNLO']*100.) )
    text += "<tr> <tr> <td> pdf err +/-: &nbsp;&nbsp;&nbsp;&nbsp; </td>\n"
    for ord in ['LO', 'NLO', 'NNLO']:
        try:
            text += wrtab("%4.1f %%" % (res_dict['pdf'+ord]/res_dict[ord]*100.) )
        except ZeroDivisionError:
            text += wrtab(0.)
#    text += wrtab("%4.1f %%" % (res_dict['pdfLO']/res_dict['LO']*100.) )
#    text += wrtab("%4.1f %%" % (res_dict['pdfNLO']/res_dict['NLO']*100.) )
#    text += wrtab("%4.1f %%" % (res_dict['pdfNNLO']/res_dict['NNLO']*100.) )
    text += "<tr> </table> <br>\n"
    text += "<br><hr>You can find <a href=\"http://vbf-nnlo.phys.ucl.ac.be/vbf_ewpar.html\">here</a> the list of EW parameters used for the computation<br><br>\n"
    text += "Click <a href =\"http://vbf-nnlo.phys.ucl.ac.be/vbf.html\">here</a> to go back to the VBF @ NNLO page<br><br>\n"
    text += "</center></BODY></HTML>"
    
print text




