import pkg_resources

modules = (
    "numpy",
    "ipywidgets",
    "ipykernel",
    "IPython",
    "spacy",
    "nltk",
    "langid",
    "pycountry",
    "pyenchant",
    "lxml",
    "matplotlib",
    "networkx",
    "pillow",
    "wordcloud",
    "pyocr",
    "scipy",
    "scikit-image",
)

print('installed modules:')

for module in sorted(modules):
    try:
        print('    ' + str(pkg_resources.get_distribution(module)))
    except pkg_resources.DistributionNotFound:
        print('    %s n/a' % module)

import locale

print('system locale:', locale.getdefaultlocale())
