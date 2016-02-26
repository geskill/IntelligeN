using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApplication1
{
    public partial class Main : Form
    {

        IntelligeN.IIntelligeN2009 IntelligeNInstance;

        public Main()
        {
            InitializeComponent();
        }

        private void Main_Load(object sender, EventArgs e)
        {

        }

        private void bStartIntelligeN_Click(object sender, EventArgs e)
        {
            IntelligeNInstance = new IntelligeN.IntelligeN2009();
        }

        private void bClose_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CloseTabSheet(IntelligeNInstance.ActiveTabSheetIndex());

            IntelligeNInstance = null;
        }

        private void bLoadXML_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.OpenTabSheet(tbFileName.Text);
        }

        private void bSearchFile_Click(object sender, EventArgs e)
        {
            if (openFileDialog.ShowDialog() == DialogResult.OK)
                tbFileName.Text = openFileDialog.FileName;
        }

        private void bStatus_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
            {
                if (IntelligeNInstance.IsCrawlerActive())
                    lCrawlerStatus.Text = "Active";
                else
                    lCrawlerStatus.Text = "Inactive";

                if (IntelligeNInstance.IsFileHosterActive())
                    lHosterStatus.Text = "Active";
                else
                    lHosterStatus.Text = "Inactive";

                if (IntelligeNInstance.IsPublishActive())
                    lPublishStatus.Text = "Active";
                else
                    lPublishStatus.Text = "Inactive";
            }
        }

        private void bStartCrawler_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CallCrawler(IntelligeNInstance.ActiveTabSheetIndex());
        }

        private void bStartRemoteImageUpload_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CallImageHosterRemoteUpload(IntelligeNInstance.ActiveTabSheetIndex());
        }

        private void bDirectlinksHosterCheck_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CallFileHosterCheck(IntelligeNInstance.ActiveTabSheetIndex());
        }

        private void bStartCrypter_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CallCrypterCrypt(IntelligeNInstance.ActiveTabSheetIndex());
        }

        private void bStartPublish_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.CallPublish(IntelligeNInstance.ActiveTabSheetIndex());
        }

        private void bSaveFile_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.SaveTabSheet(IntelligeNInstance.ActiveTabSheetIndex());
        }

    }
}
