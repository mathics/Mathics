"""
Obviously, pdflatex fails (goes into an infinite loop) when there are no mathics_*_.tex files for the Asymptote graphics. That's why we create a 99 such files before running pdflatex.
"""

def main():
    for index in range(1, 100):
        with open('mathics_%d_.tex' % index, 'wt') as img:
            img.write('-Graphics-')

if __name__ == '__main__':
    main()