namespace WindowsFormsApplication1
{
    partial class Main
    {
        /// <summary>
        /// Erforderliche Designervariable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Verwendete Ressourcen bereinigen.
        /// </summary>
        /// <param name="disposing">True, wenn verwaltete Ressourcen gelöscht werden sollen; andernfalls False.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Vom Windows Form-Designer generierter Code

        /// <summary>
        /// Erforderliche Methode für die Designerunterstützung.
        /// Der Inhalt der Methode darf nicht mit dem Code-Editor geändert werden.
        /// </summary>
        private void InitializeComponent()
        {
            this.bStartIntelligeN = new System.Windows.Forms.Button();
            this.bClose = new System.Windows.Forms.Button();
            this.bLoadXML = new System.Windows.Forms.Button();
            this.tbFileName = new System.Windows.Forms.TextBox();
            this.bSearchFile = new System.Windows.Forms.Button();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.lCrawlerLabel = new System.Windows.Forms.Label();
            this.lCrawlerStatus = new System.Windows.Forms.Label();
            this.lHosterLabel = new System.Windows.Forms.Label();
            this.bStatus = new System.Windows.Forms.Button();
            this.lHosterStatus = new System.Windows.Forms.Label();
            this.lPublishLabel = new System.Windows.Forms.Label();
            this.lPublishStatus = new System.Windows.Forms.Label();
            this.bStartCrawler = new System.Windows.Forms.Button();
            this.bStartRemoteImageUpload = new System.Windows.Forms.Button();
            this.bDirectlinksHosterCheck = new System.Windows.Forms.Button();
            this.bStartCrypter = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.bStartPublish = new System.Windows.Forms.Button();
            this.bSaveFile = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // bStartIntelligeN
            // 
            this.bStartIntelligeN.Location = new System.Drawing.Point(12, 12);
            this.bStartIntelligeN.Name = "bStartIntelligeN";
            this.bStartIntelligeN.Size = new System.Drawing.Size(97, 23);
            this.bStartIntelligeN.TabIndex = 0;
            this.bStartIntelligeN.Text = "Start IntelligeN";
            this.bStartIntelligeN.UseVisualStyleBackColor = true;
            this.bStartIntelligeN.Click += new System.EventHandler(this.bStartIntelligeN_Click);
            // 
            // bClose
            // 
            this.bClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.bClose.Location = new System.Drawing.Point(541, 12);
            this.bClose.Name = "bClose";
            this.bClose.Size = new System.Drawing.Size(101, 23);
            this.bClose.TabIndex = 1;
            this.bClose.Text = "Close active tab";
            this.bClose.UseVisualStyleBackColor = true;
            this.bClose.Click += new System.EventHandler(this.bClose_Click);
            // 
            // bLoadXML
            // 
            this.bLoadXML.Location = new System.Drawing.Point(34, 41);
            this.bLoadXML.Name = "bLoadXML";
            this.bLoadXML.Size = new System.Drawing.Size(75, 23);
            this.bLoadXML.TabIndex = 2;
            this.bLoadXML.Text = "Load XML";
            this.bLoadXML.UseVisualStyleBackColor = true;
            this.bLoadXML.Click += new System.EventHandler(this.bLoadXML_Click);
            // 
            // tbFileName
            // 
            this.tbFileName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tbFileName.Location = new System.Drawing.Point(115, 43);
            this.tbFileName.Name = "tbFileName";
            this.tbFileName.Size = new System.Drawing.Size(392, 20);
            this.tbFileName.TabIndex = 3;
            // 
            // bSearchFile
            // 
            this.bSearchFile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.bSearchFile.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.bSearchFile.Location = new System.Drawing.Point(513, 41);
            this.bSearchFile.Name = "bSearchFile";
            this.bSearchFile.Size = new System.Drawing.Size(26, 23);
            this.bSearchFile.TabIndex = 4;
            this.bSearchFile.TabStop = false;
            this.bSearchFile.Text = "...";
            this.bSearchFile.UseVisualStyleBackColor = true;
            this.bSearchFile.Click += new System.EventHandler(this.bSearchFile_Click);
            // 
            // openFileDialog
            // 
            this.openFileDialog.FileName = "openFileDialog";
            // 
            // lCrawlerLabel
            // 
            this.lCrawlerLabel.AutoSize = true;
            this.lCrawlerLabel.Location = new System.Drawing.Point(115, 76);
            this.lCrawlerLabel.Name = "lCrawlerLabel";
            this.lCrawlerLabel.Size = new System.Drawing.Size(45, 13);
            this.lCrawlerLabel.TabIndex = 5;
            this.lCrawlerLabel.Text = "Crawler:";
            // 
            // lCrawlerStatus
            // 
            this.lCrawlerStatus.AutoSize = true;
            this.lCrawlerStatus.Location = new System.Drawing.Point(115, 89);
            this.lCrawlerStatus.Name = "lCrawlerStatus";
            this.lCrawlerStatus.Size = new System.Drawing.Size(0, 13);
            this.lCrawlerStatus.TabIndex = 6;
            // 
            // lHosterLabel
            // 
            this.lHosterLabel.AutoSize = true;
            this.lHosterLabel.Location = new System.Drawing.Point(195, 76);
            this.lHosterLabel.Name = "lHosterLabel";
            this.lHosterLabel.Size = new System.Drawing.Size(41, 13);
            this.lHosterLabel.TabIndex = 7;
            this.lHosterLabel.Text = "Hoster:";
            // 
            // bStatus
            // 
            this.bStatus.Location = new System.Drawing.Point(34, 84);
            this.bStatus.Name = "bStatus";
            this.bStatus.Size = new System.Drawing.Size(75, 23);
            this.bStatus.TabIndex = 8;
            this.bStatus.Text = "Status";
            this.bStatus.UseVisualStyleBackColor = true;
            this.bStatus.Click += new System.EventHandler(this.bStatus_Click);
            // 
            // lHosterStatus
            // 
            this.lHosterStatus.AutoSize = true;
            this.lHosterStatus.Location = new System.Drawing.Point(195, 89);
            this.lHosterStatus.Name = "lHosterStatus";
            this.lHosterStatus.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lHosterStatus.Size = new System.Drawing.Size(0, 13);
            this.lHosterStatus.TabIndex = 9;
            // 
            // lPublishLabel
            // 
            this.lPublishLabel.AutoSize = true;
            this.lPublishLabel.Location = new System.Drawing.Point(285, 76);
            this.lPublishLabel.Name = "lPublishLabel";
            this.lPublishLabel.Size = new System.Drawing.Size(44, 13);
            this.lPublishLabel.TabIndex = 10;
            this.lPublishLabel.Text = "Publish:";
            // 
            // lPublishStatus
            // 
            this.lPublishStatus.AutoSize = true;
            this.lPublishStatus.Location = new System.Drawing.Point(285, 89);
            this.lPublishStatus.Name = "lPublishStatus";
            this.lPublishStatus.Size = new System.Drawing.Size(0, 13);
            this.lPublishStatus.TabIndex = 11;
            // 
            // bStartCrawler
            // 
            this.bStartCrawler.Location = new System.Drawing.Point(34, 140);
            this.bStartCrawler.Name = "bStartCrawler";
            this.bStartCrawler.Size = new System.Drawing.Size(75, 23);
            this.bStartCrawler.TabIndex = 12;
            this.bStartCrawler.Text = "Start Crawler";
            this.bStartCrawler.UseVisualStyleBackColor = true;
            this.bStartCrawler.Click += new System.EventHandler(this.bStartCrawler_Click);
            // 
            // bStartRemoteImageUpload
            // 
            this.bStartRemoteImageUpload.Location = new System.Drawing.Point(115, 140);
            this.bStartRemoteImageUpload.Name = "bStartRemoteImageUpload";
            this.bStartRemoteImageUpload.Size = new System.Drawing.Size(139, 23);
            this.bStartRemoteImageUpload.TabIndex = 13;
            this.bStartRemoteImageUpload.Text = "Remote Image Upload";
            this.bStartRemoteImageUpload.UseVisualStyleBackColor = true;
            this.bStartRemoteImageUpload.Click += new System.EventHandler(this.bStartRemoteImageUpload_Click);
            // 
            // bDirectlinksHosterCheck
            // 
            this.bDirectlinksHosterCheck.Location = new System.Drawing.Point(260, 140);
            this.bDirectlinksHosterCheck.Name = "bDirectlinksHosterCheck";
            this.bDirectlinksHosterCheck.Size = new System.Drawing.Size(139, 23);
            this.bDirectlinksHosterCheck.TabIndex = 14;
            this.bDirectlinksHosterCheck.Text = "Directlinks Hoster Check";
            this.bDirectlinksHosterCheck.UseVisualStyleBackColor = true;
            this.bDirectlinksHosterCheck.Click += new System.EventHandler(this.bDirectlinksHosterCheck_Click);
            // 
            // bStartCrypter
            // 
            this.bStartCrypter.Location = new System.Drawing.Point(405, 140);
            this.bStartCrypter.Name = "bStartCrypter";
            this.bStartCrypter.Size = new System.Drawing.Size(75, 23);
            this.bStartCrypter.TabIndex = 15;
            this.bStartCrypter.Text = "Start Crypter";
            this.bStartCrypter.UseVisualStyleBackColor = true;
            this.bStartCrypter.Click += new System.EventHandler(this.bStartCrypter_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.BackColor = System.Drawing.SystemColors.Control;
            this.label1.Location = new System.Drawing.Point(115, 27);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(52, 13);
            this.label1.TabIndex = 16;
            this.label1.Text = "Filename:";
            // 
            // bStartPublish
            // 
            this.bStartPublish.Location = new System.Drawing.Point(486, 140);
            this.bStartPublish.Name = "bStartPublish";
            this.bStartPublish.Size = new System.Drawing.Size(75, 23);
            this.bStartPublish.TabIndex = 17;
            this.bStartPublish.Text = "Start Publish";
            this.bStartPublish.UseVisualStyleBackColor = true;
            this.bStartPublish.Click += new System.EventHandler(this.bStartPublish_Click);
            // 
            // bSaveFile
            // 
            this.bSaveFile.Location = new System.Drawing.Point(567, 140);
            this.bSaveFile.Name = "bSaveFile";
            this.bSaveFile.Size = new System.Drawing.Size(75, 23);
            this.bSaveFile.TabIndex = 18;
            this.bSaveFile.Text = "Save";
            this.bSaveFile.UseVisualStyleBackColor = true;
            this.bSaveFile.Click += new System.EventHandler(this.bSaveFile_Click);
            // 
            // Main
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(654, 192);
            this.Controls.Add(this.bSaveFile);
            this.Controls.Add(this.bStartPublish);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.bStartCrypter);
            this.Controls.Add(this.bDirectlinksHosterCheck);
            this.Controls.Add(this.bStartRemoteImageUpload);
            this.Controls.Add(this.bStartCrawler);
            this.Controls.Add(this.lPublishStatus);
            this.Controls.Add(this.lPublishLabel);
            this.Controls.Add(this.lHosterStatus);
            this.Controls.Add(this.bStatus);
            this.Controls.Add(this.lHosterLabel);
            this.Controls.Add(this.lCrawlerStatus);
            this.Controls.Add(this.lCrawlerLabel);
            this.Controls.Add(this.bSearchFile);
            this.Controls.Add(this.tbFileName);
            this.Controls.Add(this.bLoadXML);
            this.Controls.Add(this.bClose);
            this.Controls.Add(this.bStartIntelligeN);
            this.Name = "Main";
            this.Text = "OLE Demo";
            this.Load += new System.EventHandler(this.Main_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button bStartIntelligeN;
        private System.Windows.Forms.Button bClose;
        private System.Windows.Forms.Button bLoadXML;
        private System.Windows.Forms.TextBox tbFileName;
        private System.Windows.Forms.Button bSearchFile;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.Button bStatus;
        private System.Windows.Forms.Label lCrawlerLabel;
        private System.Windows.Forms.Label lCrawlerStatus;
        private System.Windows.Forms.Label lHosterLabel;
        private System.Windows.Forms.Label lHosterStatus;
        private System.Windows.Forms.Label lPublishLabel;
        private System.Windows.Forms.Label lPublishStatus;
        private System.Windows.Forms.Button bStartCrawler;
        private System.Windows.Forms.Button bStartRemoteImageUpload;
        private System.Windows.Forms.Button bDirectlinksHosterCheck;
        private System.Windows.Forms.Button bStartCrypter;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button bStartPublish;
        private System.Windows.Forms.Button bSaveFile;
    }
}

