import pkg_resources

modules = (
    "numpy",
    "ipywidgets",
    "ipykernel",
    "IPython",
    "nltk",
    "langid",
    "pycountry",
    "pyenchant",
    "lxml",
    "matplotlib",
    "networkx",
    "PIL",
    "wordcloud",
    "pycor",
    "scipy",
    "skimage",
)

print('installed modules:')

for module in modules:
    try:
        print('    ' + str(pkg_resources.get_distribution(module)))
    except pkg_resources.DistributionNotFound:
        print('    %s n/a' % module)
