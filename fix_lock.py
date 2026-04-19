import json
import re

with open('renv.lock') as f:
    lock = json.load(f)

std = {'Package','Version','Source','Repository','Hash','Requirements',
       'RemoteType','RemoteHost','RemoteUsername','RemoteRepo','RemoteRef',
       'RemoteSha','RemoteSubdir','RemotePkgRef'}

for pkg in lock.get('Packages', {}).values():
    # Remove non-standard fields that rsconnect can't parse
    for k in list(pkg.keys()):
        if k not in std:
            del pkg[k]

    # Normalize RSPM Linux binary repos -> CRAN so shinyapps.io can find packages
    if pkg.get('Source') == 'Repository':
        repo = pkg.get('Repository', '')
        if 'packagemanager.posit.co' in repo or 'packagemanager.rstudio.com' in repo:
            pkg['Repository'] = 'CRAN'

    # Strip Linux binary patch suffix from versions (e.g. "0.2.1-1" -> "0.2.1")
    version = pkg.get('Version', '')
    if re.match(r'^\d+\.\d+.*-\d+$', version):
        pkg['Version'] = re.sub(r'-\d+$', '', version)

with open('renv.lock', 'w') as f:
    json.dump(lock, f, indent=2)
