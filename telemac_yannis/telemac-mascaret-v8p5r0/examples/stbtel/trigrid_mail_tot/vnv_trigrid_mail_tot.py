
"""
Validation script for trigrid_mail_tot
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['stbtel']

    def _pre(self):
        """
        Defining the studies
        """

        # trigrid_mail_tot scalar mode
        self.add_study('vnv_1',
                       'stbtel',
                       'stb_tot.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
